flow_quality_from_cv <- function(cv_pct, method = "volumetric") {
  if (!is.finite(cv_pct)) return(ifelse(method == "installed", "A", "D"))
  if (method == "volumetric") {
    if (cv_pct <= 5) return("B_HIGH_REPEATABILITY")
    if (cv_pct <= 10) return("B_MODERATE_REPEATABILITY")
    return("C_LOW_REPEATABILITY")
  }
  if (method == "float") {
    if (cv_pct <= 10) return("C_APPROXIMATE")
    return("D_UNSTABLE_APPROXIMATION")
  }
  "D"
}

validate_volumetric_context <- function(context = NULL) {
  required_bool <- c(
    "capture_complete", "container_calibrated", "measurement_location_known",
    "no_diversions", "stable_flow", "safe_measurement"
  )
  required_text <- c(
    "measurement_location", "measurement_date", "responsible", "container_id"
  )
  if (is.null(context) || nrow(as.data.frame(context)) < 1L) {
    return(list(
      complete = FALSE,
      missing_fields = c(required_bool, required_text),
      context = data.frame(stringsAsFactors = FALSE)
    ))
  }
  z <- as.data.frame(context, stringsAsFactors = FALSE)[1, , drop = FALSE]
  missing_columns <- setdiff(c(required_bool, required_text), names(z))
  false_bool <- character(0)
  for (nm in intersect(required_bool, names(z))) {
    if (!isTRUE(parse_bool(z[[nm]], default = FALSE))) false_bool <- c(false_bool, nm)
  }
  empty_text <- character(0)
  for (nm in intersect(required_text, names(z))) {
    if (!text_present(z[[nm]])) empty_text <- c(empty_text, nm)
  }
  missing_fields <- unique(c(missing_columns, false_bool, empty_text))
  list(
    complete = length(missing_fields) == 0L,
    missing_fields = missing_fields,
    context = z
  )
}

summarise_volumetric_measurement <- function(replicates, context = NULL) {
  x <- as.data.frame(replicates, stringsAsFactors = FALSE)
  if (!all(c("captured_liters", "elapsed_seconds") %in% names(x))) {
    stop("Volumetric data require captured_liters and elapsed_seconds.", call. = FALSE)
  }
  x$captured_liters <- as.numeric(x$captured_liters)
  x$elapsed_seconds <- as.numeric(x$elapsed_seconds)
  if (nrow(x) < 3L) stop("At least three volumetric repetitions are required.", call. = FALSE)
  if (any(!is.finite(x$captured_liters)) || any(!is.finite(x$elapsed_seconds)) ||
      any(x$captured_liters <= 0) || any(x$elapsed_seconds <= 0)) {
    stop("Volumetric repetitions must be positive and finite.", call. = FALSE)
  }
  x$flow_lps <- x$captured_liters / x$elapsed_seconds
  mean_flow <- mean(x$flow_lps)
  cv <- 100 * stats::sd(x$flow_lps) / mean_flow
  repeatability_class <- flow_quality_from_cv(cv, "volumetric")
  context_check <- validate_volumetric_context(context)
  operational_verified <- isTRUE(context_check$complete) &&
    repeatability_class %in% c("B_HIGH_REPEATABILITY", "B_MODERATE_REPEATABILITY")
  quality_class <- if (operational_verified && repeatability_class == "B_HIGH_REPEATABILITY") {
    "B_VERIFIED_VOLUMETRIC"
  } else if (operational_verified) {
    "B_VERIFIED_MODERATE_REPEATABILITY"
  } else if (repeatability_class %in% c("B_HIGH_REPEATABILITY", "B_MODERATE_REPEATABILITY")) {
    "B_REPEATABLE_CONTEXT_INCOMPLETE"
  } else {
    repeatability_class
  }
  missing_text <- if (length(context_check$missing_fields)) {
    paste(context_check$missing_fields, collapse = ";")
  } else {
    ""
  }
  list(
    replicates = x,
    context = context_check$context,
    summary = data.frame(
      method_id = "volumetric_container",
      n_replicates = nrow(x),
      median_flow_lps = stats::median(x$flow_lps),
      mean_flow_lps = mean_flow,
      minimum_flow_lps = min(x$flow_lps),
      maximum_flow_lps = max(x$flow_lps),
      cv_pct = cv,
      repeatability_class = repeatability_class,
      context_complete = context_check$complete,
      operational_flow_verified = operational_verified,
      quality_class = quality_class,
      context_missing_fields = missing_text,
      stringsAsFactors = FALSE
    )
  )
}

summarise_float_measurement <- function(replicates) {
  x <- as.data.frame(replicates, stringsAsFactors = FALSE)
  req <- c("distance_m", "travel_time_s", "channel_width_m", "mean_depth_m", "correction_factor")
  if (!all(req %in% names(x))) stop("Float data missing required columns.", call. = FALSE)
  x[req] <- lapply(x[req], as.numeric)
  if (nrow(x) < 3L) stop("At least three float repetitions are required.", call. = FALSE)
  if (any(vapply(x[req], function(v) any(!is.finite(v) | v <= 0), logical(1)))) {
    stop("Float-method inputs must be positive and finite.", call. = FALSE)
  }
  x$surface_velocity_m_s <- x$distance_m / x$travel_time_s
  x$corrected_velocity_m_s <- x$surface_velocity_m_s * x$correction_factor
  x$cross_section_area_m2 <- x$channel_width_m * x$mean_depth_m
  x$flow_lps <- x$cross_section_area_m2 * x$corrected_velocity_m_s * 1000
  mean_flow <- mean(x$flow_lps)
  cv <- 100 * stats::sd(x$flow_lps) / mean_flow
  list(
    replicates = x,
    summary = data.frame(
      method_id = "float_open_channel",
      n_replicates = nrow(x),
      median_flow_lps = stats::median(x$flow_lps),
      mean_flow_lps = mean_flow,
      minimum_flow_lps = min(x$flow_lps),
      maximum_flow_lps = max(x$flow_lps),
      cv_pct = cv,
      repeatability_class = flow_quality_from_cv(cv, "float"),
      context_complete = FALSE,
      operational_flow_verified = FALSE,
      quality_class = flow_quality_from_cv(cv, "float"),
      context_missing_fields = "technical_validation_required",
      stringsAsFactors = FALSE
    )
  )
}

summarise_meter_difference <- function(start_volume_m3, end_volume_m3, elapsed_seconds,
                                       meter_validated = FALSE) {
  vals <- as.numeric(c(start_volume_m3, end_volume_m3, elapsed_seconds))
  if (any(!is.finite(vals)) || end_volume_m3 <= start_volume_m3 || elapsed_seconds <= 0) {
    stop("Invalid meter-difference inputs.", call. = FALSE)
  }
  flow <- 1000 * (end_volume_m3 - start_volume_m3) / elapsed_seconds
  data.frame(
    method_id = "meter_volume_difference",
    n_replicates = 1L,
    median_flow_lps = flow,
    mean_flow_lps = flow,
    minimum_flow_lps = flow,
    maximum_flow_lps = flow,
    cv_pct = NA_real_,
    repeatability_class = "NOT_APPLICABLE_SINGLE_INTERVAL",
    context_complete = isTRUE(meter_validated),
    operational_flow_verified = isTRUE(meter_validated),
    quality_class = if (isTRUE(meter_validated)) "A_VERIFIED_METER" else "A_IF_METER_VALID",
    context_missing_fields = if (isTRUE(meter_validated)) "" else "meter_validation_required",
    stringsAsFactors = FALSE
  )
}

turn_volume_m3 <- function(flow_lps, duration_hours) {
  if (!is.finite(flow_lps) || flow_lps <= 0 || !is.finite(duration_hours) || duration_hours < 0) {
    return(NA_real_)
  }
  flow_lps * 3.6 * duration_hours
}

hours_required_for_volume <- function(volume_m3, flow_lps) {
  if (!is.finite(volume_m3) || volume_m3 < 0 || !is.finite(flow_lps) || flow_lps <= 0) {
    return(NA_real_)
  }
  volume_m3 / (flow_lps * 3.6)
}
