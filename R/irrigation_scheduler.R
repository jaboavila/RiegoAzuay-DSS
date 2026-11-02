minimum_net_irrigation_for_segment <- function(weather_segment, profile_row, soil_row,
                                               days_after_planting_start,
                                               depletion_start_mm,
                                               previous_root_depth_m = NA_real_,
                                               adjust_p_for_etc = TRUE,
                                               runoff_fraction = 0,
                                               subsoil_water_mode = "field_capacity",
                                               subsoil_theta_value = NA_real_,
                                               tolerance_mm = 1e-5) {
  empty_schedule <- data.frame(date = as.Date(character(0)), gross_irrigation_mm = numeric(0))
  zero <- simulate_daily_balance(
    weather_segment, profile_row, soil_row,
    days_after_planting_start = days_after_planting_start,
    initial_depletion_mm = depletion_start_mm,
    delivery_efficiency = 1,
    irrigation_schedule = empty_schedule,
    adjust_p_for_etc = adjust_p_for_etc,
    runoff_fraction = runoff_fraction,
    subsoil_water_mode = subsoil_water_mode,
    subsoil_theta_value = subsoil_theta_value,
    initial_previous_root_depth_m = previous_root_depth_m
  )
  if (!any(zero$stress_flag)) {
    return(list(required_net_mm = 0, bridge_possible = TRUE, trace = zero,
                full_refill_net_mm = 0))
  }

  upper <- max(0, zero$depletion_start_mm[[1]] - zero$effective_precip_mm_day[[1]])
  schedule_upper <- data.frame(date = as.Date(weather_segment$date[[1]]), gross_irrigation_mm = upper)
  full <- simulate_daily_balance(
    weather_segment, profile_row, soil_row,
    days_after_planting_start = days_after_planting_start,
    initial_depletion_mm = depletion_start_mm,
    delivery_efficiency = 1,
    irrigation_schedule = schedule_upper,
    adjust_p_for_etc = adjust_p_for_etc,
    runoff_fraction = runoff_fraction,
    subsoil_water_mode = subsoil_water_mode,
    subsoil_theta_value = subsoil_theta_value,
    initial_previous_root_depth_m = previous_root_depth_m
  )
  if (any(full$stress_flag)) {
    return(list(required_net_mm = upper, bridge_possible = FALSE, trace = full,
                full_refill_net_mm = upper))
  }

  lo <- 0
  hi <- upper
  for (iter in seq_len(60L)) {
    mid <- (lo + hi) / 2
    z <- simulate_daily_balance(
      weather_segment, profile_row, soil_row,
      days_after_planting_start = days_after_planting_start,
      initial_depletion_mm = depletion_start_mm,
      delivery_efficiency = 1,
      irrigation_schedule = data.frame(date = as.Date(weather_segment$date[[1]]), gross_irrigation_mm = mid),
      adjust_p_for_etc = adjust_p_for_etc,
      runoff_fraction = runoff_fraction,
      subsoil_water_mode = subsoil_water_mode,
      subsoil_theta_value = subsoil_theta_value,
      initial_previous_root_depth_m = previous_root_depth_m
    )
    if (any(z$stress_flag)) lo <- mid else hi <- mid
    if ((hi - lo) <= tolerance_mm) break
  }
  final <- simulate_daily_balance(
    weather_segment, profile_row, soil_row,
    days_after_planting_start = days_after_planting_start,
    initial_depletion_mm = depletion_start_mm,
    delivery_efficiency = 1,
    irrigation_schedule = data.frame(date = as.Date(weather_segment$date[[1]]), gross_irrigation_mm = hi),
    adjust_p_for_etc = adjust_p_for_etc,
    runoff_fraction = runoff_fraction,
    subsoil_water_mode = subsoil_water_mode,
    subsoil_theta_value = subsoil_theta_value,
    initial_previous_root_depth_m = previous_root_depth_m
  )
  list(required_net_mm = hi, bridge_possible = TRUE, trace = final,
       full_refill_net_mm = upper)
}

normalise_next_turn_map <- function(next_turn_by_date = character(0)) {
  if (is.null(next_turn_by_date) || length(next_turn_by_date) == 0L) return(character(0))
  if (is.null(names(next_turn_by_date)) || any(!nzchar(names(next_turn_by_date)))) {
    stop("next_turn_by_date must be a named vector: current_turn_date = next_turn_date.", call. = FALSE)
  }
  values <- as.character(as.Date(as.character(next_turn_by_date)))
  if (anyNA(as.Date(values))) stop("next_turn_by_date contains invalid dates.", call. = FALSE)
  names(values) <- as.character(as.Date(names(next_turn_by_date)))
  values
}

resolve_turn_coverage <- function(x_dates, current_index, allowed_dates,
                                  next_turn_by_date = character(0)) {
  current_date <- as.Date(x_dates[[current_index]])
  current_chr <- as.character(current_date)
  x_dates <- as.Date(x_dates)
  allowed_dates <- as.Date(allowed_dates)
  future_allowed <- sort(allowed_dates[allowed_dates > current_date])
  next_turn <- as.Date(NA)
  source <- "horizon_end"
  if (length(future_allowed)) {
    next_turn <- future_allowed[[1]]
    source <- "next_allowed_date"
  } else if (current_chr %in% names(next_turn_by_date)) {
    next_turn <- as.Date(next_turn_by_date[[current_chr]])
    source <- "declared_next_turn"
  }

  horizon_end <- max(x_dates)
  if (is.na(next_turn)) {
    coverage_end <- horizon_end
    weather_covers <- FALSE
    basis <- "HORIZON_END_ONLY"
  } else {
    target_end <- next_turn - 1
    weather_covers <- horizon_end >= target_end
    coverage_end <- if (weather_covers) target_end else horizon_end
    basis <- if (weather_covers) "NEXT_TURN_MINUS_ONE" else "INCOMPLETE_TO_NEXT_TURN"
  }
  idx <- which(seq_along(x_dates) >= current_index & x_dates <= coverage_end)
  if (!length(idx)) idx <- current_index
  list(
    segment_end_index = max(idx),
    next_turn_date = next_turn,
    coverage_end_date = coverage_end,
    weather_covers_to_next_turn = weather_covers,
    coverage_basis = basis,
    next_turn_source = source
  )
}

plan_weekly_irrigation <- function(weather, profile_row, soil_row,
                                   days_after_planting_start,
                                   initial_depletion_mm,
                                   area_ha,
                                   delivery_efficiency,
                                   allowed_dates = NULL,
                                   flow_lps = NA_real_,
                                   max_hours_by_date = numeric(0),
                                   next_turn_by_date = character(0),
                                   reporting_horizon_end_date = as.Date(NA),
                                   adjust_p_for_etc = TRUE,
                                   runoff_fraction = 0,
                                   subsoil_water_mode = "field_capacity",
                                   subsoil_theta_value = NA_real_,
                                   tolerance_mm = 1e-5) {
  x <- as.data.frame(weather, stringsAsFactors = FALSE)
  x$date <- as.Date(x$date)
  if (anyNA(x$date)) stop("weather contains invalid dates.", call. = FALSE)
  if (!is.finite(area_ha) || area_ha <= 0) stop("area_ha must be positive.", call. = FALSE)
  if (is.null(allowed_dates) || length(allowed_dates) == 0L) allowed_dates <- x$date
  allowed_dates <- as.Date(allowed_dates)
  allowed_chr <- as.character(allowed_dates)
  next_turn_by_date <- normalise_next_turn_map(next_turn_by_date)
  reporting_horizon_end_date <- as.Date(reporting_horizon_end_date)
  if (is.na(reporting_horizon_end_date)) reporting_horizon_end_date <- max(x$date)

  events <- list()
  daily <- list()
  event_counter <- 1L
  dr_prev <- as.numeric(initial_depletion_mm)
  root_curve <- build_root_depth_curve(profile_row)
  first_crop_day <- as.integer(days_after_planting_start)
  if (first_crop_day < 1L || first_crop_day > length(root_curve)) {
    stop("days_after_planting_start is outside the crop cycle.", call. = FALSE)
  }
  zr_prev <- root_curve[[first_crop_day]]

  for (i in seq_len(nrow(x))) {
    current_date <- x$date[[i]]
    current_chr <- as.character(current_date)
    applied_gross_mm <- 0
    event_record <- NULL

    if (current_chr %in% allowed_chr) {
      coverage <- resolve_turn_coverage(x$date, i, allowed_dates, next_turn_by_date)
      segment <- x[i:coverage$segment_end_index, , drop = FALSE]
      req <- minimum_net_irrigation_for_segment(
        segment, profile_row, soil_row,
        days_after_planting_start = days_after_planting_start + i - 1L,
        depletion_start_mm = dr_prev,
        previous_root_depth_m = zr_prev,
        adjust_p_for_etc = adjust_p_for_etc,
        runoff_fraction = runoff_fraction,
        subsoil_water_mode = subsoil_water_mode,
        subsoil_theta_value = subsoil_theta_value,
        tolerance_mm = tolerance_mm
      )
      required_net_mm <- req$required_net_mm
      required_gross_mm <- required_net_mm / delivery_efficiency
      required_volume_m3 <- required_gross_mm * area_ha * 10
      required_hours <- if (is.finite(flow_lps) && flow_lps > 0) {
        required_volume_m3 / (flow_lps * 3.6)
      } else NA_real_
      hours_available <- if (current_chr %in% names(max_hours_by_date)) {
        as.numeric(max_hours_by_date[[current_chr]])
      } else NA_real_
      turn_capacity_m3 <- if (is.finite(flow_lps) && flow_lps > 0 && is.finite(hours_available)) {
        flow_lps * 3.6 * hours_available
      } else NA_real_
      capacity_limited <- is.finite(turn_capacity_m3) && required_volume_m3 > turn_capacity_m3 + 1e-9
      deliverable_volume_m3 <- if (is.finite(turn_capacity_m3)) min(required_volume_m3, turn_capacity_m3) else required_volume_m3
      unmet_volume_m3 <- max(required_volume_m3 - deliverable_volume_m3, 0)
      deliverable_hours <- if (is.finite(flow_lps) && flow_lps > 0) deliverable_volume_m3 / (flow_lps * 3.6) else NA_real_
      unmet_hours <- if (is.finite(required_hours) && is.finite(hours_available)) max(required_hours - hours_available, 0) else NA_real_
      applied_gross_mm <- deliverable_volume_m3 / (area_ha * 10)

      actual_segment <- simulate_daily_balance(
        segment, profile_row, soil_row,
        days_after_planting_start = days_after_planting_start + i - 1L,
        initial_depletion_mm = dr_prev,
        delivery_efficiency = delivery_efficiency,
        irrigation_schedule = data.frame(date = current_date, gross_irrigation_mm = applied_gross_mm),
        adjust_p_for_etc = adjust_p_for_etc,
        runoff_fraction = runoff_fraction,
        subsoil_water_mode = subsoil_water_mode,
        subsoil_theta_value = subsoil_theta_value,
        initial_previous_root_depth_m = zr_prev
      )
      bridge_possible_after_delivery <- !any(actual_segment$stress_flag)
      residual_stress_days <- sum(actual_segment$stress_flag)
      covers_to_next_turn_if_full_refill <- if (coverage$weather_covers_to_next_turn) req$bridge_possible else NA
      covers_to_next_turn_after_delivery <- if (coverage$weather_covers_to_next_turn) bridge_possible_after_delivery else NA

      event_record <- data.frame(
        event_id = paste0("TURN_", sprintf("%02d", event_counter)),
        date = current_date,
        irrigation_recommended = required_gross_mm > tolerance_mm,
        reporting_horizon_end_date = reporting_horizon_end_date,
        coverage_end_date = coverage$coverage_end_date,
        next_turn_date = coverage$next_turn_date,
        coverage_basis = coverage$coverage_basis,
        next_turn_source = coverage$next_turn_source,
        weather_covers_to_next_turn = coverage$weather_covers_to_next_turn,
        required_net_mm = required_net_mm,
        required_gross_mm = required_gross_mm,
        required_volume_m3 = required_volume_m3,
        required_liters = required_volume_m3 * 1000,
        deliverable_volume_m3 = deliverable_volume_m3,
        deliverable_liters = deliverable_volume_m3 * 1000,
        unmet_volume_m3 = unmet_volume_m3,
        unmet_liters = unmet_volume_m3 * 1000,
        flow_lps = flow_lps,
        required_hours = required_hours,
        deliverable_hours = deliverable_hours,
        hours_available = hours_available,
        unmet_hours = unmet_hours,
        turn_capacity_m3 = turn_capacity_m3,
        capacity_limited = capacity_limited,
        covers_coverage_window_if_full_refill = req$bridge_possible,
        covers_coverage_window_after_delivery = bridge_possible_after_delivery,
        covers_to_next_turn_if_full_refill = covers_to_next_turn_if_full_refill,
        covers_to_next_turn_after_delivery = covers_to_next_turn_after_delivery,
        residual_stress_days_within_coverage = residual_stress_days,
        applied_gross_mm_for_balance = applied_gross_mm,
        recommended_volume_m3 = required_volume_m3,
        recommended_liters = required_volume_m3 * 1000,
        stringsAsFactors = FALSE
      )
      events[[event_counter]] <- event_record
      event_counter <- event_counter + 1L
    }

    one_day <- simulate_daily_balance(
      x[i, , drop = FALSE], profile_row, soil_row,
      days_after_planting_start = days_after_planting_start + i - 1L,
      initial_depletion_mm = dr_prev,
      delivery_efficiency = delivery_efficiency,
      irrigation_schedule = data.frame(date = current_date, gross_irrigation_mm = applied_gross_mm),
      adjust_p_for_etc = adjust_p_for_etc,
      runoff_fraction = runoff_fraction,
      subsoil_water_mode = subsoil_water_mode,
      subsoil_theta_value = subsoil_theta_value,
      initial_previous_root_depth_m = zr_prev
    )
    one_day$within_reporting_horizon <- current_date <= reporting_horizon_end_date
    one_day$allowed_turn_day <- current_chr %in% allowed_chr
    one_day$turn_conflict <- one_day$stress_flag && !(current_chr %in% allowed_chr)
    one_day$capacity_limited_event <- if (!is.null(event_record)) event_record$capacity_limited[[1]] else FALSE
    one_day$required_volume_m3_event <- if (!is.null(event_record)) event_record$required_volume_m3[[1]] else 0
    one_day$deliverable_volume_m3_event <- if (!is.null(event_record)) event_record$deliverable_volume_m3[[1]] else 0
    one_day$unmet_volume_m3_event <- if (!is.null(event_record)) event_record$unmet_volume_m3[[1]] else 0
    daily[[i]] <- one_day
    dr_prev <- one_day$depletion_end_mm[[1]]
    zr_prev <- one_day$root_depth_m[[1]]
  }

  event_df <- if (length(events)) do.call(rbind, events) else data.frame(
    event_id = character(0), date = as.Date(character(0)), irrigation_recommended = logical(0),
    reporting_horizon_end_date = as.Date(character(0)), coverage_end_date = as.Date(character(0)),
    next_turn_date = as.Date(character(0)), coverage_basis = character(0), next_turn_source = character(0),
    weather_covers_to_next_turn = logical(0), required_net_mm = numeric(0),
    required_gross_mm = numeric(0), required_volume_m3 = numeric(0), required_liters = numeric(0),
    deliverable_volume_m3 = numeric(0), deliverable_liters = numeric(0),
    unmet_volume_m3 = numeric(0), unmet_liters = numeric(0), flow_lps = numeric(0),
    required_hours = numeric(0), deliverable_hours = numeric(0), hours_available = numeric(0),
    unmet_hours = numeric(0), turn_capacity_m3 = numeric(0), capacity_limited = logical(0),
    covers_coverage_window_if_full_refill = logical(0), covers_coverage_window_after_delivery = logical(0),
    covers_to_next_turn_if_full_refill = logical(0), covers_to_next_turn_after_delivery = logical(0),
    residual_stress_days_within_coverage = integer(0), applied_gross_mm_for_balance = numeric(0),
    recommended_volume_m3 = numeric(0), recommended_liters = numeric(0), stringsAsFactors = FALSE
  )

  list(daily = do.call(rbind, daily), events = event_df)
}

plan_weather_ensemble <- function(weather_ensemble, profile_row, soil_row,
                                  days_after_planting_start,
                                  initial_depletion_mm,
                                  area_ha,
                                  delivery_efficiency,
                                  allowed_dates,
                                  flow_lps = NA_real_,
                                  max_hours_by_date = numeric(0),
                                  next_turn_by_date = character(0),
                                  reporting_horizon_end_date = as.Date(NA),
                                  adjust_p_for_etc = TRUE,
                                  runoff_fraction = 0,
                                  subsoil_water_mode = "field_capacity",
                                  subsoil_theta_value = NA_real_) {
  x <- as.data.frame(weather_ensemble, stringsAsFactors = FALSE)
  if (!"weather_scenario" %in% names(x)) stop("weather_ensemble requires weather_scenario.", call. = FALSE)
  scenarios <- unique(x$weather_scenario)
  plans <- list()
  summaries <- list()
  for (s in scenarios) {
    z <- plan_weekly_irrigation(
      x[x$weather_scenario == s, c("date", "eto_mm_day", "precip_mm_day"), drop = FALSE],
      profile_row, soil_row,
      days_after_planting_start, initial_depletion_mm, area_ha, delivery_efficiency,
      allowed_dates, flow_lps, max_hours_by_date,
      next_turn_by_date = next_turn_by_date,
      reporting_horizon_end_date = reporting_horizon_end_date,
      adjust_p_for_etc = adjust_p_for_etc,
      runoff_fraction = runoff_fraction,
      subsoil_water_mode = subsoil_water_mode,
      subsoil_theta_value = subsoil_theta_value
    )
    z$daily$weather_scenario <- s
    if (nrow(z$events)) z$events$weather_scenario <- s
    plans[[s]] <- z
    required_m3 <- if (nrow(z$events)) sum(z$events$required_volume_m3) else 0
    deliverable_m3 <- if (nrow(z$events)) sum(z$events$deliverable_volume_m3) else 0
    unmet_m3 <- if (nrow(z$events)) sum(z$events$unmet_volume_m3) else 0
    required_hours <- if (nrow(z$events) && all(is.finite(z$events$required_hours))) sum(z$events$required_hours) else NA_real_
    deliverable_hours <- if (nrow(z$events) && all(is.finite(z$events$deliverable_hours))) sum(z$events$deliverable_hours) else NA_real_
    unmet_hours <- if (nrow(z$events) && all(is.finite(z$events$unmet_hours))) sum(z$events$unmet_hours) else NA_real_
    hours_available <- if (nrow(z$events) && all(is.finite(z$events$hours_available))) sum(z$events$hours_available) else NA_real_
    coverage_verified <- nrow(z$events) > 0L && all(z$events$weather_covers_to_next_turn)
    covers_next_turn <- if (coverage_verified) all(z$events$covers_to_next_turn_after_delivery) else NA
    summaries[[s]] <- data.frame(
      weather_scenario = s,
      required_volume_m3 = required_m3,
      deliverable_volume_m3 = deliverable_m3,
      unmet_volume_m3 = unmet_m3,
      required_liters = required_m3 * 1000,
      deliverable_liters = deliverable_m3 * 1000,
      unmet_liters = unmet_m3 * 1000,
      required_hours = required_hours,
      deliverable_hours = deliverable_hours,
      hours_available = hours_available,
      unmet_hours = unmet_hours,
      irrigation_events = if (nrow(z$events)) sum(z$events$irrigation_recommended) else 0L,
      any_stress = any(z$daily$stress_flag),
      any_turn_conflict = any(z$daily$turn_conflict),
      any_capacity_limit = any(z$daily$capacity_limited_event),
      weather_covers_to_next_turn = coverage_verified,
      covers_to_next_turn_after_delivery = covers_next_turn,
      reporting_horizon_end_date = as.Date(reporting_horizon_end_date),
      validation_horizon_end_date = max(as.Date(z$daily$date)),
      recommended_volume_m3 = required_m3,
      recommended_liters = required_m3 * 1000,
      stringsAsFactors = FALSE
    )
  }
  summary_df <- do.call(rbind, summaries)
  central_row <- summary_df[summary_df$weather_scenario == "central", , drop = FALSE]
  if (nrow(central_row) != 1L) central_row <- summary_df[1, , drop = FALSE]
  min_v <- min(summary_df$required_volume_m3)
  max_v <- max(summary_df$required_volume_m3)
  central_v <- central_row$required_volume_m3[[1]]
  relative_width <- if (central_v > 0) (max_v - min_v) / central_v else Inf
  confidence <- if (any(summary_df$any_turn_conflict | summary_df$any_capacity_limit) ||
                  any(!summary_df$weather_covers_to_next_turn)) {
    "LOW"
  } else if (relative_width <= 0.20) {
    "HIGH"
  } else if (relative_width <= 0.50) {
    "MEDIUM"
  } else {
    "LOW"
  }
  envelope <- data.frame(
    central_scenario = central_row$weather_scenario[[1]],
    central_required_volume_m3 = central_v,
    central_deliverable_volume_m3 = central_row$deliverable_volume_m3[[1]],
    central_unmet_volume_m3 = central_row$unmet_volume_m3[[1]],
    minimum_required_volume_m3 = min_v,
    maximum_required_volume_m3 = max_v,
    central_required_liters = central_v * 1000,
    minimum_required_liters = min_v * 1000,
    maximum_required_liters = max_v * 1000,
    weather_covers_to_next_turn = central_row$weather_covers_to_next_turn[[1]],
    covers_to_next_turn_after_delivery = central_row$covers_to_next_turn_after_delivery[[1]],
    reporting_horizon_end_date = central_row$reporting_horizon_end_date[[1]],
    validation_horizon_end_date = central_row$validation_horizon_end_date[[1]],
    confidence = confidence,
    central_volume_m3 = central_v,
    minimum_volume_m3 = min_v,
    maximum_volume_m3 = max_v,
    central_liters = central_v * 1000,
    minimum_liters = min_v * 1000,
    maximum_liters = max_v * 1000,
    stringsAsFactors = FALSE
  )
  list(plans = plans, scenario_summary = summary_df, envelope = envelope)
}
