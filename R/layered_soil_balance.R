validate_layered_soil <- function(layers) {
  x <- as.data.frame(layers, stringsAsFactors = FALSE)
  required <- c("depth_top_m", "depth_bottom_m", "theta_fc", "theta_wp")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Layered soil is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  x[required] <- lapply(x[required], as.numeric)
  if (any(!is.finite(as.matrix(x[required])))) stop("Layered soil contains nonfinite values.", call. = FALSE)
  x <- x[order(x$depth_top_m, x$depth_bottom_m), , drop = FALSE]
  if (any(x$depth_bottom_m <= x$depth_top_m)) stop("Soil layer bottom must exceed top.", call. = FALSE)
  if (any(x$theta_fc <= x$theta_wp)) stop("Each soil layer requires theta_fc > theta_wp.", call. = FALSE)
  if (min(x$depth_top_m) > 1e-9) stop("Layered soil must begin at the surface.", call. = FALSE)
  if (nrow(x) > 1L && any(abs(x$depth_top_m[-1] - x$depth_bottom_m[-nrow(x)]) > 1e-9)) {
    stop("Layered soil contains gaps or overlaps.", call. = FALSE)
  }
  x
}

layer_overlap_m <- function(layers, root_depth_m) {
  if (!is.finite(root_depth_m) || root_depth_m <= 0) stop("root_depth_m must be positive.", call. = FALSE)
  pmax(0, pmin(layers$depth_bottom_m, root_depth_m) - layers$depth_top_m)
}

layered_taw_mm <- function(layers, root_depth_m) {
  x <- validate_layered_soil(layers)
  overlap <- layer_overlap_m(x, root_depth_m)
  if (sum(overlap) + 1e-9 < root_depth_m) stop("Soil layers do not cover the complete root depth.", call. = FALSE)
  sum(1000 * (x$theta_fc - x$theta_wp) * overlap)
}

layered_equivalent_theta <- function(layers, root_depth_m, field = c("theta_fc", "theta_wp")) {
  field <- match.arg(field)
  x <- validate_layered_soil(layers)
  overlap <- layer_overlap_m(x, root_depth_m)
  if (sum(overlap) + 1e-9 < root_depth_m) stop("Soil layers do not cover the complete root depth.", call. = FALSE)
  sum(x[[field]] * overlap) / sum(overlap)
}

layered_added_taw_mm <- function(layers, previous_depth_m, new_depth_m) {
  if (new_depth_m <= previous_depth_m + 1e-12) return(0)
  layered_taw_mm(layers, new_depth_m) - layered_taw_mm(layers, previous_depth_m)
}

layered_root_expansion_adjustment_mm <- function(layers, previous_depth_m, new_depth_m,
                                                 mode, depletion_previous_mm,
                                                 measured_theta = NA_real_) {
  if (new_depth_m <= previous_depth_m + 1e-12) return(0)
  x <- validate_layered_soil(layers)
  add_overlap <- pmax(0, pmin(x$depth_bottom_m, new_depth_m) - pmax(x$depth_top_m, previous_depth_m))
  mode <- tolower(trimws(as.character(mode)))
  if (mode == "field_capacity") return(0)
  added_taw <- sum(1000 * (x$theta_fc - x$theta_wp) * add_overlap)
  if (mode == "wilting_point") return(added_taw)
  if (mode == "same_relative_depletion") {
    previous_taw <- layered_taw_mm(x, previous_depth_m)
    fraction <- if (previous_taw > 0) clamp_value(depletion_previous_mm / previous_taw, 0, 1) else 0
    return(fraction * added_taw)
  }
  if (mode == "measured_theta") {
    theta <- as.numeric(measured_theta)
    if (!is.finite(theta)) stop("measured_theta is required for layered root expansion.", call. = FALSE)
    deficits <- pmax(x$theta_fc - theta, 0)
    return(sum(1000 * deficits * add_overlap))
  }
  stop("Unknown layered subsoil mode: ", mode, call. = FALSE)
}

simulate_daily_balance_layered <- function(weather, profile_row, soil_layers,
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
  if (anyNA(x$date) || any(!is.finite(x$eto_mm_day)) || any(!is.finite(x$precip_mm_day))) stop("Weather contains invalid values.", call. = FALSE)
  if (!is.finite(delivery_efficiency) || delivery_efficiency <= 0 || delivery_efficiency > 1) stop("delivery_efficiency must be in (0,1].", call. = FALSE)
  if (!is.finite(runoff_fraction) || runoff_fraction < 0 || runoff_fraction >= 1) stop("runoff_fraction must be in [0,1).", call. = FALSE)
  soil_layers <- validate_layered_soil(soil_layers)

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
  out <- vector("list", nrow(x))
  dr_prev <- as.numeric(initial_depletion_mm)
  previous_zr <- as.numeric(initial_previous_root_depth_m)

  for (i in seq_len(nrow(x))) {
    crop_day <- as.integer(days_after_planting_start + i - 1L)
    if (crop_day < 1L || crop_day > length(kc_curve)) stop("Crop day outside crop cycle.", call. = FALSE)
    kc <- kc_curve[[crop_day]]
    zr <- root_curve[[crop_day]]
    if (!is.finite(previous_zr)) previous_zr <- zr
    previous_zr <- min(previous_zr, zr)
    root_adjustment <- layered_root_expansion_adjustment_mm(
      soil_layers, previous_zr, zr, subsoil_water_mode, dr_prev,
      measured_theta = subsoil_theta_value
    )
    taw <- layered_taw_mm(soil_layers, zr)
    dr_start_unclamped <- dr_prev + root_adjustment
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
    residual <- dr_end - (dr_start - peff - net_irr + etc_adj + deep_percolation)

    out[[i]] <- data.frame(
      date = x$date[[i]], crop_day = crop_day,
      crop_stage_es = crop_stage_name(profile_row, crop_day, "es"),
      crop_stage_en = crop_stage_name(profile_row, crop_day, "en"),
      kc = kc, root_depth_previous_m = previous_zr, root_depth_m = zr,
      root_depth_increment_m = max(zr - previous_zr, 0),
      subsoil_water_mode = as.character(subsoil_water_mode),
      root_expansion_depletion_adjustment_mm = root_adjustment,
      theta_fc_equivalent = layered_equivalent_theta(soil_layers, zr, "theta_fc"),
      theta_wp_equivalent = layered_equivalent_theta(soil_layers, zr, "theta_wp"),
      taw_mm = taw, p_adjusted = p_adj, raw_mm = raw,
      depletion_start_unclamped_mm = dr_start_unclamped,
      depletion_start_mm = dr_start,
      eto_mm_day = x$eto_mm_day[[i]], etc_potential_mm_day = etc_pot,
      precipitation_mm_day = x$precip_mm_day[[i]], effective_precip_mm_day = peff,
      irrigation_gross_mm_day = gross_irr, irrigation_net_mm_day = net_irr,
      depletion_after_wetting_mm = dr_after_wetting, ks = ks,
      etc_adjusted_mm_day = etc_adj, deep_percolation_mm_day = deep_percolation,
      depletion_end_mm = dr_end, balance_residual_mm = residual,
      stress_flag = dr_after_wetting > raw + 1e-9,
      stringsAsFactors = FALSE
    )
    dr_prev <- dr_end
    previous_zr <- zr
  }
  do.call(rbind, out)
}
