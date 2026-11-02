calc_taw_mm <- function(theta_fc, theta_wp, root_depth_m) {
  if (!is.finite(theta_fc) || !is.finite(theta_wp) || theta_fc <= theta_wp) {
    stop("theta_fc must be greater than theta_wp.", call. = FALSE)
  }
  if (!is.finite(root_depth_m) || root_depth_m <= 0) stop("root_depth_m must be positive.", call. = FALSE)
  1000 * (theta_fc - theta_wp) * root_depth_m
}

adjust_depletion_fraction <- function(p_table, etc_potential_mm_day, enabled = TRUE) {
  if (!enabled) return(as.numeric(p_table))
  clamp_value(as.numeric(p_table) + 0.04 * (5 - as.numeric(etc_potential_mm_day)), 0.1, 0.8)
}

calc_raw_mm <- function(taw_mm, p_adjusted) {
  taw_mm * p_adjusted
}

calc_ks <- function(depletion_after_wetting_mm, taw_mm, raw_mm) {
  if (depletion_after_wetting_mm <= raw_mm + 1e-12) return(1)
  if (taw_mm <= raw_mm + 1e-12) return(0)
  clamp_value((taw_mm - depletion_after_wetting_mm) / (taw_mm - raw_mm), 0, 1)
}

initial_depletion_mm_from_mode <- function(mode, value, soil_row, root_depth_m, p_table = NA_real_, etc_mm_day = NA_real_) {
  mode <- tolower(trimws(as.character(mode)))
  taw <- calc_taw_mm(soil_row$theta_fc[[1]], soil_row$theta_wp[[1]], root_depth_m)
  if (mode == "mm") return(clamp_value(as.numeric(value), 0, taw))
  if (mode == "fraction_taw") return(clamp_value(as.numeric(value), 0, 1) * taw)
  if (mode == "field_capacity") return(0)
  if (mode == "measured_theta") {
    theta <- as.numeric(value)
    return(clamp_value(1000 * (soil_row$theta_fc[[1]] - theta) * root_depth_m, 0, taw))
  }
  if (mode == "fraction_raw") {
    if (!is.finite(p_table)) stop("p_table is required for fraction_raw mode.", call. = FALSE)
    p_adj <- adjust_depletion_fraction(p_table, etc_mm_day %||% 5, enabled = is.finite(etc_mm_day))
    return(clamp_value(as.numeric(value), 0, 1) * calc_raw_mm(taw, p_adj))
  }
  stop("Unknown initial depletion mode: ", mode, call. = FALSE)
}

resolve_subsoil_theta <- function(mode, soil_row, depletion_previous_mm,
                                  previous_root_depth_m, subsoil_theta_value = NA_real_) {
  mode <- tolower(trimws(as.character(mode)))
  theta_fc <- as.numeric(soil_row$theta_fc[[1]])
  theta_wp <- as.numeric(soil_row$theta_wp[[1]])

  if (mode == "field_capacity") return(theta_fc)
  if (mode == "wilting_point") return(theta_wp)
  if (mode == "measured_theta") {
    theta <- as.numeric(subsoil_theta_value)
    if (!is.finite(theta) || theta < 0 || theta > theta_fc) {
      stop("measured subsoil theta must be finite and between 0 and theta_fc.", call. = FALSE)
    }
    return(theta)
  }
  if (mode == "same_relative_depletion") {
    taw_previous <- calc_taw_mm(theta_fc, theta_wp, previous_root_depth_m)
    fraction <- if (taw_previous > 0) clamp_value(depletion_previous_mm / taw_previous, 0, 1) else 0
    return(theta_fc - fraction * (theta_fc - theta_wp))
  }
  stop("Unknown subsoil_water_mode: ", mode, call. = FALSE)
}

root_expansion_adjustment_mm <- function(delta_root_depth_m, soil_row,
                                         subsoil_theta_assumed) {
  if (!is.finite(delta_root_depth_m) || delta_root_depth_m < 0) {
    stop("delta_root_depth_m must be finite and nonnegative.", call. = FALSE)
  }
  if (delta_root_depth_m == 0) return(0)
  theta_fc <- as.numeric(soil_row$theta_fc[[1]])
  max(0, 1000 * (theta_fc - as.numeric(subsoil_theta_assumed)) * delta_root_depth_m)
}

simulate_daily_balance <- function(weather, profile_row, soil_row,
                                   days_after_planting_start,
                                   initial_depletion_mm,
                                   delivery_efficiency = 1,
                                   irrigation_schedule = NULL,
                                   adjust_p_for_etc = TRUE,
                                   runoff_fraction = 0,
                                   subsoil_water_mode = "field_capacity",
                                   subsoil_theta_value = NA_real_,
                                   initial_previous_root_depth_m = NA_real_) {
  x <- as.data.frame(weather, stringsAsFactors = FALSE)
  required <- c("date", "eto_mm_day", "precip_mm_day")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Weather table missing: ", paste(missing, collapse = ", "), call. = FALSE)
  x$date <- as.Date(x$date)
  x$eto_mm_day <- as.numeric(x$eto_mm_day)
  x$precip_mm_day <- as.numeric(x$precip_mm_day)
  if (anyNA(x$date) || any(!is.finite(x$eto_mm_day)) || any(!is.finite(x$precip_mm_day))) {
    stop("Weather contains invalid dates or numeric values.", call. = FALSE)
  }
  if (!is.finite(delivery_efficiency) || delivery_efficiency <= 0 || delivery_efficiency > 1) {
    stop("delivery_efficiency must be in (0,1].", call. = FALSE)
  }
  if (!is.finite(runoff_fraction) || runoff_fraction < 0 || runoff_fraction >= 1) {
    stop("runoff_fraction must be in [0,1).", call. = FALSE)
  }

  irr <- setNames(rep(0, nrow(x)), as.character(x$date))
  if (!is.null(irrigation_schedule) && nrow(irrigation_schedule) > 0L) {
    irrigation_schedule$date <- as.Date(irrigation_schedule$date)
    irrigation_schedule$gross_irrigation_mm <- as.numeric(irrigation_schedule$gross_irrigation_mm)
    idx <- match(as.character(irrigation_schedule$date), names(irr))
    valid <- !is.na(idx)
    irr[idx[valid]] <- irrigation_schedule$gross_irrigation_mm[valid]
  }

  kc_curve <- build_kc_curve(profile_row)
  root_curve <- build_root_depth_curve(profile_row)
  n <- nrow(x)
  out <- vector("list", n)
  dr_prev <- as.numeric(initial_depletion_mm)
  previous_zr <- as.numeric(initial_previous_root_depth_m)

  for (i in seq_len(n)) {
    crop_day <- as.integer(days_after_planting_start + i - 1L)
    if (crop_day < 1L || crop_day > length(kc_curve)) {
      stop("Crop day outside the configured crop cycle: ", crop_day, call. = FALSE)
    }
    kc <- kc_curve[[crop_day]]
    zr <- root_curve[[crop_day]]
    if (!is.finite(previous_zr)) previous_zr <- zr
    previous_zr <- min(previous_zr, zr)
    delta_zr <- max(zr - previous_zr, 0)
    theta_sub <- resolve_subsoil_theta(
      subsoil_water_mode, soil_row, dr_prev, previous_zr,
      subsoil_theta_value = subsoil_theta_value
    )
    root_adjustment <- root_expansion_adjustment_mm(delta_zr, soil_row, theta_sub)

    taw <- calc_taw_mm(soil_row$theta_fc[[1]], soil_row$theta_wp[[1]], zr)
    dr_before_root_adjustment <- dr_prev
    dr_start_unclamped <- dr_before_root_adjustment + root_adjustment
    dr_start <- clamp_value(dr_start_unclamped, 0, taw)
    etc_pot <- x$eto_mm_day[[i]] * kc
    p_adj <- adjust_depletion_fraction(profile_row$p_table[[1]], etc_pot, adjust_p_for_etc)
    raw <- calc_raw_mm(taw, p_adj)
    peff <- max(x$precip_mm_day[[i]] * (1 - runoff_fraction), 0)
    gross_irr <- irr[[as.character(x$date[[i]])]]
    net_irr <- gross_irr * delivery_efficiency
    dr_after_wetting <- max(dr_start - peff - net_irr, 0)
    ks <- calc_ks(dr_after_wetting, taw, raw)
    etc_adj <- ks * etc_pot
    preliminary_end <- dr_start - peff - net_irr + etc_adj
    deep_percolation <- max(-preliminary_end, 0)
    dr_end <- clamp_value(preliminary_end, 0, taw)
    balance_residual <- dr_end - (dr_start - peff - net_irr + etc_adj + deep_percolation)

    out[[i]] <- data.frame(
      date = x$date[[i]],
      crop_day = crop_day,
      crop_stage_es = crop_stage_name(profile_row, crop_day, "es"),
      crop_stage_en = crop_stage_name(profile_row, crop_day, "en"),
      kc = kc,
      root_depth_previous_m = previous_zr,
      root_depth_m = zr,
      root_depth_increment_m = delta_zr,
      subsoil_water_mode = as.character(subsoil_water_mode),
      subsoil_theta_assumed = theta_sub,
      root_expansion_depletion_adjustment_mm = root_adjustment,
      theta_fc = soil_row$theta_fc[[1]],
      theta_wp = soil_row$theta_wp[[1]],
      taw_mm = taw,
      p_adjusted = p_adj,
      raw_mm = raw,
      depletion_before_root_adjustment_mm = dr_before_root_adjustment,
      depletion_start_unclamped_mm = dr_start_unclamped,
      depletion_start_mm = dr_start,
      eto_mm_day = x$eto_mm_day[[i]],
      etc_potential_mm_day = etc_pot,
      precipitation_mm_day = x$precip_mm_day[[i]],
      effective_precip_mm_day = peff,
      irrigation_gross_mm_day = gross_irr,
      irrigation_net_mm_day = net_irr,
      depletion_after_wetting_mm = dr_after_wetting,
      ks = ks,
      etc_adjusted_mm_day = etc_adj,
      deep_percolation_mm_day = deep_percolation,
      depletion_end_mm = dr_end,
      balance_residual_mm = balance_residual,
      stress_flag = dr_after_wetting > raw + 1e-9,
      stringsAsFactors = FALSE
    )
    dr_prev <- dr_end
    previous_zr <- zr
  }
  do.call(rbind, out)
}
