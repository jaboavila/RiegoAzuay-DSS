url_encode_value <- function(x) {
  utils::URLencode(as.character(x), reserved = TRUE)
}

build_query_url <- function(endpoint, params) {
  params <- params[!vapply(params, function(x) is.null(x) || length(x) == 0L || all(is.na(x)), logical(1))]
  query <- paste0(names(params), "=", vapply(params, url_encode_value, character(1)), collapse = "&")
  paste0(endpoint, "?", query)
}

read_key_value_config <- function(path) {
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  if (!all(c("parameter", "value") %in% names(x))) {
    stop("Configuration requires parameter and value columns: ", path, call. = FALSE)
  }
  if (anyDuplicated(x$parameter)) stop("Configuration parameters must be unique.", call. = FALSE)
  setNames(as.list(x$value), x$parameter)
}

forecast_receipt_from_json <- function(raw_path, receipt_path, source_id, request_url,
                                       acquisition_mode = "live_download",
                                       recovered_from_path = "",
                                       retrieved_at_utc = NA_character_,
                                       http_status = NA_integer_,
                                       content_type = "application/json") {
  if (!file.exists(raw_path)) stop("Archived JSON not found: ", raw_path, call. = FALSE)
  parsed <- jsonlite::fromJSON(raw_path, simplifyVector = TRUE)
  if (is.na(retrieved_at_utc) || !nzchar(retrieved_at_utc)) {
    retrieved_at_utc <- format(file.info(raw_path)$mtime, tz = "UTC", usetz = TRUE)
  }
  receipt <- data.frame(
    source_id = source_id,
    acquisition_mode = acquisition_mode,
    recovered_from_path = recovered_from_path,
    request_url = request_url,
    retrieved_at_utc = retrieved_at_utc,
    http_status = http_status,
    content_type = content_type,
    content_length_header = "",
    raw_file = gsub("\\\\", "/", raw_path),
    raw_size_bytes = file.info(raw_path)$size,
    raw_sha256 = digest::digest(raw_path, algo = "sha256", file = TRUE),
    response_latitude = parsed$latitude %||% NA_real_,
    response_longitude = parsed$longitude %||% NA_real_,
    response_elevation_m = parsed$elevation %||% NA_real_,
    response_timezone = parsed$timezone %||% "",
    response_timezone_abbreviation = parsed$timezone_abbreviation %||% "",
    response_utc_offset_seconds = parsed$utc_offset_seconds %||% NA_real_,
    response_model = paste(parsed$model %||% "", collapse = ";"),
    hourly_et0_unit = (parsed$hourly_units$et0_fao_evapotranspiration %||% ""),
    hourly_precipitation_unit = (parsed$hourly_units$precipitation %||% ""),
    generationtime_ms = parsed$generationtime_ms %||% NA_real_,
    stringsAsFactors = FALSE
  )
  write_csv_utf8(receipt, receipt_path)
  list(json = parsed, receipt = receipt, raw_path = raw_path, receipt_path = receipt_path)
}

archive_http_json <- function(url, raw_path, receipt_path, source_id, timeout_seconds = 180) {
  dir.create(dirname(raw_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(receipt_path), recursive = TRUE, showWarnings = FALSE)
  req <- httr2::request(url) |>
    httr2::req_user_agent("RiegoAzuay-DSS/2.1.4 academic-research") |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE, backoff = function(tries) min(60, 2 ^ tries))
  retrieved_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  resp <- httr2::req_perform(req)
  raw <- httr2::resp_body_raw(resp)
  if (!length(raw)) stop("The forecast response is empty: ", source_id, call. = FALSE)
  writeBin(raw, raw_path)
  parsed <- try(jsonlite::fromJSON(rawToChar(raw), simplifyVector = TRUE), silent = TRUE)
  if (inherits(parsed, "try-error")) stop("The archived response is not valid JSON: ", source_id, call. = FALSE)
  headers <- httr2::resp_headers(resp)
  forecast_receipt_from_json(
    raw_path = raw_path,
    receipt_path = receipt_path,
    source_id = source_id,
    request_url = url,
    acquisition_mode = "live_download",
    recovered_from_path = "",
    retrieved_at_utc = retrieved_at,
    http_status = httr2::resp_status(resp),
    content_type = headers[["content-type"]] %||% ""
  )
}

read_archived_json <- function(path) {
  if (!file.exists(path)) stop("Archived JSON not found: ", path, call. = FALSE)
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

read_receipt_field <- function(path, field, default = "") {
  if (!file.exists(path)) return(default)
  x <- try(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), silent = TRUE)
  if (inherits(x, "try-error") || !field %in% names(x) || !nrow(x)) return(default)
  value <- as.character(x[[field]][[1]])
  if (is.na(value)) default else value
}

find_previous_forecast_raw_pair <- function(search_root = dirname(normalizePath(getwd(), winslash = "/", mustWork = TRUE))) {
  explicit <- Sys.getenv("FORECAST_RECOVER_DIR", unset = "")
  package_dirs <- character(0)
  if (nzchar(explicit) && dir.exists(explicit)) {
    package_dirs <- normalizePath(explicit, winslash = "/", mustWork = TRUE)
  } else {
    dirs <- list.dirs(search_root, recursive = TRUE, full.names = TRUE)
    package_dirs <- dirs[grepl("Article4_DailyBalance_ForecastArchive_v2\\.1\\.[0-2]", basename(dirs))]
  }
  if (!length(package_dirs)) return(NULL)
  candidates <- list()
  for (pkg in unique(package_dirs)) {
    runs_base <- file.path(pkg, "outputs", "forecast_runs")
    if (!dir.exists(runs_base)) next
    run_dirs <- list.dirs(runs_base, recursive = FALSE, full.names = TRUE)
    for (run_dir in run_dirs) {
      det <- file.path(run_dir, "raw", "openmeteo_ecmwf_ifs_deterministic.json")
      ens <- file.path(run_dir, "raw", "openmeteo_ecmwf_ifs025_ensemble.json")
      if (file.exists(det) && file.exists(ens)) {
        candidates[[length(candidates) + 1L]] <- list(
          run_dir = run_dir,
          det_raw = det,
          ens_raw = ens,
          det_receipt = file.path(run_dir, "receipts", "deterministic_request_receipt.csv"),
          ens_receipt = file.path(run_dir, "receipts", "ensemble_request_receipt.csv"),
          mtime = max(file.info(c(det, ens))$mtime)
        )
      }
    }
  }
  if (!length(candidates)) return(NULL)
  candidates[[which.max(vapply(candidates, function(x) as.numeric(x$mtime), numeric(1)))]]
}

recover_previous_forecast_pair <- function(raw_dir, receipt_dir, urls) {
  source <- find_previous_forecast_raw_pair()
  if (is.null(source)) return(NULL)
  det_dest <- file.path(raw_dir, "openmeteo_ecmwf_ifs_deterministic.json")
  ens_dest <- file.path(raw_dir, "openmeteo_ecmwf_ifs025_ensemble.json")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(receipt_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source$det_raw, det_dest, overwrite = TRUE) ||
      !file.copy(source$ens_raw, ens_dest, overwrite = TRUE)) {
    stop("Failed to recover archived forecast JSON from a prior v2.1.0-v2.1.3 run.", call. = FALSE)
  }
  det_url <- read_receipt_field(source$det_receipt, "request_url", urls$deterministic)
  ens_url <- read_receipt_field(source$ens_receipt, "request_url", urls$ensemble)
  det_time <- read_receipt_field(source$det_receipt, "retrieved_at_utc", "")
  ens_time <- read_receipt_field(source$ens_receipt, "retrieved_at_utc", "")
  det <- forecast_receipt_from_json(
    det_dest, file.path(receipt_dir, "deterministic_request_receipt.csv"),
    "ECMWF_IFS_HRES_VIA_OPEN_METEO", det_url,
    acquisition_mode = "recovered_from_failed_prior_run",
    recovered_from_path = normalizePath(source$det_raw, winslash = "/", mustWork = TRUE),
    retrieved_at_utc = det_time
  )
  ens <- forecast_receipt_from_json(
    ens_dest, file.path(receipt_dir, "ensemble_request_receipt.csv"),
    "ECMWF_IFS025_ENSEMBLE_VIA_OPEN_METEO", ens_url,
    acquisition_mode = "recovered_from_failed_prior_run",
    recovered_from_path = normalizePath(source$ens_raw, winslash = "/", mustWork = TRUE),
    retrieved_at_utc = ens_time
  )
  list(deterministic = det, ensemble = ens, source_run_dir = source$run_dir)
}

validate_hourly_core <- function(obj, required_variables, source_label = "forecast") {
  if (is.null(obj$hourly) || is.null(obj$hourly$time)) {
    stop(source_label, " does not contain hourly time-series data.", call. = FALSE)
  }
  times <- as.character(obj$hourly$time)
  if (!length(times) || anyNA(times) || any(!nzchar(times))) {
    stop(source_label, " contains invalid hourly timestamps.", call. = FALSE)
  }
  missing_vars <- setdiff(required_variables, names(obj$hourly))
  if (length(missing_vars)) {
    stop(source_label, " is missing variables: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }
  n <- length(times)
  bad_lengths <- required_variables[vapply(required_variables, function(nm) length(obj$hourly[[nm]]) != n, logical(1))]
  if (length(bad_lengths)) {
    stop(source_label, " has misaligned arrays: ", paste(bad_lengths, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

empty_forecast_value_diagnostics <- function() {
  data.frame(
    source_label = character(0), member_id = character(0), time_local = character(0),
    date = as.Date(character(0)), variable = character(0), original_value = numeric(0),
    issue = character(0), policy_action = character(0), in_required_horizon = logical(0),
    fatal = logical(0), stringsAsFactors = FALSE
  )
}

forecast_dates_from_object <- function(obj, source_label = "forecast") {
  if (is.null(obj$hourly) || is.null(obj$hourly$time)) {
    stop(source_label, " does not contain hourly timestamps.", call. = FALSE)
  }
  dates <- as.Date(substr(as.character(obj$hourly$time), 1, 10))
  if (!length(dates) || anyNA(dates)) {
    stop(source_label, " contains invalid hourly timestamps.", call. = FALSE)
  }
  sort(unique(dates))
}

apply_hourly_forecast_value_policy <- function(x, source_label,
                                                diagnostic_path = NULL,
                                                precipitation_negative_tolerance_mm = 1e-6,
                                                et0_reject_below_mm = -1,
                                                required_start_date = NULL,
                                                required_end_date = NULL,
                                                fatal_mode = c("stop", "mark")) {
  fatal_mode <- match.arg(fatal_mode)
  z <- as.data.frame(x, stringsAsFactors = FALSE)
  required <- c("time_local", "date", "et0_mm_hour", "precip_mm_hour")
  missing <- setdiff(required, names(z))
  if (length(missing)) stop("Hourly forecast table is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!"member_id" %in% names(z)) z$member_id <- "deterministic"
  z$member_id <- as.character(z$member_id)
  z$date <- as.Date(z$date)
  z$et0_mm_hour <- suppressWarnings(as.numeric(z$et0_mm_hour))
  z$precip_mm_hour <- suppressWarnings(as.numeric(z$precip_mm_hour))

  in_scope <- rep(TRUE, nrow(z))
  if (!is.null(required_start_date) && !is.null(required_end_date)) {
    required_start_date <- as.Date(required_start_date)
    required_end_date <- as.Date(required_end_date)
    in_scope <- z$date >= required_start_date & z$date <= required_end_date
  }

  diagnostics <- list(); k <- 1L
  add_diag <- function(idx, variable, issue, action, inherently_fatal) {
    if (!length(idx)) return(invisible(NULL))
    diagnostics[[k]] <<- data.frame(
      source_label = source_label,
      member_id = z$member_id[idx],
      time_local = z$time_local[idx],
      date = as.Date(z$date[idx]),
      variable = variable,
      original_value = if (variable == "et0_mm_hour") z$et0_mm_hour[idx] else z$precip_mm_hour[idx],
      issue = issue,
      policy_action = action,
      in_required_horizon = in_scope[idx],
      fatal = inherently_fatal & in_scope[idx],
      stringsAsFactors = FALSE
    )
    k <<- k + 1L
    invisible(NULL)
  }

  nonfinite_et0 <- which(!is.finite(z$et0_mm_hour))
  nonfinite_p <- which(!is.finite(z$precip_mm_hour))
  negative_et0 <- which(is.finite(z$et0_mm_hour) & z$et0_mm_hour < 0)
  sentinel_et0 <- which(is.finite(z$et0_mm_hour) & z$et0_mm_hour < et0_reject_below_mm)
  tiny_negative_p <- which(is.finite(z$precip_mm_hour) & z$precip_mm_hour < 0 &
                             z$precip_mm_hour >= -abs(precipitation_negative_tolerance_mm))
  material_negative_p <- which(is.finite(z$precip_mm_hour) &
                                 z$precip_mm_hour < -abs(precipitation_negative_tolerance_mm))

  add_diag(nonfinite_et0, "et0_mm_hour", "nonfinite_value",
           "mark_incomplete; ignore_only_if_outside_required_horizon", TRUE)
  add_diag(nonfinite_p, "precip_mm_hour", "nonfinite_value",
           "mark_incomplete; ignore_only_if_outside_required_horizon", TRUE)
  add_diag(negative_et0, "et0_mm_hour", "negative_hourly_et0", "preserve_for_24h_sum", FALSE)
  add_diag(sentinel_et0, "et0_mm_hour", "sentinel_like_or_implausible_negative_et0",
           "mark_incomplete; reject_if_required", TRUE)
  add_diag(tiny_negative_p, "precip_mm_hour", "tiny_negative_precipitation", "clamp_to_zero", FALSE)
  add_diag(material_negative_p, "precip_mm_hour", "material_negative_precipitation",
           "mark_incomplete; reject_if_required", TRUE)

  diag <- if (length(diagnostics)) do.call(rbind, diagnostics) else empty_forecast_value_diagnostics()
  if (!is.null(diagnostic_path)) write_csv_utf8(diag, diagnostic_path)

  # Invalid numeric values are never imputed. They become NA so that the day/member
  # is rejected by the completeness gate when it lies inside the required horizon.
  if (length(nonfinite_et0)) z$et0_mm_hour[nonfinite_et0] <- NA_real_
  if (length(nonfinite_p)) z$precip_mm_hour[nonfinite_p] <- NA_real_
  if (length(sentinel_et0)) z$et0_mm_hour[sentinel_et0] <- NA_real_
  if (length(material_negative_p)) z$precip_mm_hour[material_negative_p] <- NA_real_
  z$et0_negative_hour <- is.finite(z$et0_mm_hour) & z$et0_mm_hour < 0
  z$precip_clamped_hour <- FALSE
  if (length(tiny_negative_p)) {
    z$precip_mm_hour[tiny_negative_p] <- 0
    z$precip_clamped_hour[tiny_negative_p] <- TRUE
  }

  fatal <- nrow(diag) > 0L && any(diag$fatal)
  if (fatal && identical(fatal_mode, "stop")) {
    details <- if (!is.null(diagnostic_path)) paste0(" Diagnostic: ", diagnostic_path) else ""
    stop(source_label, " contains invalid values inside the required forecast horizon.", details, call. = FALSE)
  }

  safe_min <- function(v) if (any(is.finite(v))) min(v[is.finite(v)]) else NA_real_
  safe_sum_negative <- function(v) if (any(is.finite(v) & v < 0)) sum(v[is.finite(v) & v < 0]) else 0
  summary <- data.frame(
    source_label = source_label,
    rows = nrow(z),
    members = length(unique(z$member_id)),
    required_horizon_rows = sum(in_scope),
    invalid_required_horizon_rows = if (nrow(diag)) sum(diag$fatal) else 0L,
    invalid_outside_required_horizon_rows = if (nrow(diag)) sum(!diag$in_required_horizon &
      diag$issue %in% c("nonfinite_value", "sentinel_like_or_implausible_negative_et0", "material_negative_precipitation")) else 0L,
    negative_et0_hours = sum(z$et0_negative_hour, na.rm = TRUE),
    minimum_hourly_et0_mm = safe_min(z$et0_mm_hour),
    sum_negative_hourly_et0_mm = safe_sum_negative(z$et0_mm_hour),
    tiny_negative_precipitation_hours_clamped = sum(z$precip_clamped_hour, na.rm = TRUE),
    minimum_hourly_precipitation_after_policy_mm = safe_min(z$precip_mm_hour),
    precipitation_negative_tolerance_mm = precipitation_negative_tolerance_mm,
    et0_reject_below_mm = et0_reject_below_mm,
    required_start_date = if (is.null(required_start_date)) as.Date(NA) else as.Date(required_start_date),
    required_end_date = if (is.null(required_end_date)) as.Date(NA) else as.Date(required_end_date),
    fatal_mode = fatal_mode,
    stringsAsFactors = FALSE
  )
  attr(z, "forecast_value_diagnostics") <- diag
  attr(z, "forecast_value_summary") <- summary
  attr(z, "required_horizon_invalid") <- fatal
  z
}

parse_openmeteo_deterministic <- function(obj, diagnostic_path = NULL,
                                          precipitation_negative_tolerance_mm = 1e-6,
                                          et0_reject_below_mm = -1,
                                          required_start_date = NULL,
                                          required_end_date = NULL,
                                          fatal_mode = c("stop", "mark")) {
  fatal_mode <- match.arg(fatal_mode)
  validate_hourly_core(obj, c("et0_fao_evapotranspiration", "precipitation"), "deterministic forecast")
  out <- data.frame(
    time_local = as.character(obj$hourly$time),
    date = as.Date(substr(as.character(obj$hourly$time), 1, 10)),
    et0_mm_hour = suppressWarnings(as.numeric(obj$hourly$et0_fao_evapotranspiration)),
    precip_mm_hour = suppressWarnings(as.numeric(obj$hourly$precipitation)),
    stringsAsFactors = FALSE
  )
  if (anyNA(out$date)) stop("Deterministic forecast contains invalid dates.", call. = FALSE)
  apply_hourly_forecast_value_policy(
    out, "deterministic_forecast", diagnostic_path,
    precipitation_negative_tolerance_mm, et0_reject_below_mm,
    required_start_date, required_end_date, fatal_mode
  )
}

ensemble_keys <- function(hourly_names, variable) {
  pattern <- paste0("^", variable, "(_member[0-9]+)?$")
  grep(pattern, hourly_names, value = TRUE)
}

member_id_from_key <- function(key, variable) {
  if (identical(key, variable)) return("control")
  sub(paste0("^", variable, "_"), "", key)
}

parse_openmeteo_ensemble <- function(obj, diagnostic_path = NULL,
                                     precipitation_negative_tolerance_mm = 1e-6,
                                     et0_reject_below_mm = -1,
                                     required_start_date = NULL,
                                     required_end_date = NULL,
                                     fatal_mode = c("stop", "mark")) {
  fatal_mode <- match.arg(fatal_mode)
  if (is.null(obj$hourly) || is.null(obj$hourly$time)) {
    stop("Ensemble response does not contain hourly data.", call. = FALSE)
  }
  hourly_names <- names(obj$hourly)
  et0_keys <- ensemble_keys(hourly_names, "et0_fao_evapotranspiration")
  p_keys <- ensemble_keys(hourly_names, "precipitation")
  if (!length(et0_keys) || !length(p_keys)) {
    stop("Ensemble response lacks ET0 or precipitation members.", call. = FALSE)
  }
  et0_ids <- vapply(et0_keys, member_id_from_key, character(1), variable = "et0_fao_evapotranspiration")
  p_ids <- vapply(p_keys, member_id_from_key, character(1), variable = "precipitation")
  common <- intersect(et0_ids, p_ids)
  if (length(common) < 3L) stop("Fewer than three paired ensemble members were found.", call. = FALSE)
  times <- as.character(obj$hourly$time)
  dates <- as.Date(substr(times, 1, 10))
  if (anyNA(dates)) stop("Ensemble response contains invalid dates.", call. = FALSE)
  out <- vector("list", length(common))
  for (i in seq_along(common)) {
    id <- common[[i]]
    ekey <- et0_keys[match(id, et0_ids)]
    pkey <- p_keys[match(id, p_ids)]
    e <- suppressWarnings(as.numeric(obj$hourly[[ekey]]))
    p <- suppressWarnings(as.numeric(obj$hourly[[pkey]]))
    if (length(e) != length(times) || length(p) != length(times)) {
      stop("Ensemble arrays are not aligned for member ", id, call. = FALSE)
    }
    out[[i]] <- data.frame(
      member_id = id, time_local = times, date = dates,
      et0_mm_hour = e, precip_mm_hour = p, stringsAsFactors = FALSE
    )
  }
  ans <- do.call(rbind, out)
  row.names(ans) <- NULL
  apply_hourly_forecast_value_policy(
    ans, "ensemble_forecast", diagnostic_path,
    precipitation_negative_tolerance_mm, et0_reject_below_mm,
    required_start_date, required_end_date, fatal_mode
  )
}

aggregate_hourly_daily <- function(x, member_column = NULL) {
  z <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!nrow(z)) stop("Hourly forecast table is empty.", call. = FALSE)
  one_day <- function(y, member_id = NULL) {
    total_hours <- nrow(y)
    finite_et0_hours <- sum(is.finite(y$et0_mm_hour))
    finite_precip_hours <- sum(is.finite(y$precip_mm_hour))
    complete_day <- total_hours == 24L && finite_et0_hours == 24L && finite_precip_hours == 24L
    raw_eto <- if (complete_day) sum(y$et0_mm_hour) else NA_real_
    used_eto <- if (complete_day) max(raw_eto, 0) else NA_real_
    precip <- if (complete_day) sum(y$precip_mm_hour) else NA_real_
    row <- data.frame(
      date = as.Date(y$date[[1]]),
      eto_raw_sum_mm_day = raw_eto,
      eto_mm_day = used_eto,
      daily_eto_floor_applied = if (complete_day) raw_eto < 0 else NA,
      negative_et0_hours = sum(is.finite(y$et0_mm_hour) & y$et0_mm_hour < 0),
      sum_negative_hourly_et0_mm = if (any(is.finite(y$et0_mm_hour) & y$et0_mm_hour < 0))
        sum(y$et0_mm_hour[is.finite(y$et0_mm_hour) & y$et0_mm_hour < 0]) else 0,
      precip_mm_day = precip,
      tiny_negative_precipitation_hours_clamped = if ("precip_clamped_hour" %in% names(y))
        sum(y$precip_clamped_hour, na.rm = TRUE) else 0,
      hours = total_hours,
      finite_et0_hours = finite_et0_hours,
      finite_precip_hours = finite_precip_hours,
      complete_day = complete_day,
      stringsAsFactors = FALSE
    )
    if (!is.null(member_id)) row <- data.frame(member_id = member_id, row, stringsAsFactors = FALSE, check.names = FALSE)
    row
  }
  if (is.null(member_column)) {
    out <- lapply(sort(unique(z$date)), function(d) one_day(z[z$date == d, , drop = FALSE]))
  } else {
    out <- list(); k <- 1L
    for (id in unique(z[[member_column]])) {
      zz <- z[z[[member_column]] == id, , drop = FALSE]
      for (d in sort(unique(zz$date))) {
        out[[k]] <- one_day(zz[zz$date == d, , drop = FALSE], id)
        k <- k + 1L
      }
    }
  }
  ans <- do.call(rbind, out)
  row.names(ans) <- NULL
  ans
}

validate_daily_coverage <- function(daily, start_date, end_date, group_column = NULL) {
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  expected <- seq(start_date, end_date, by = "day")
  groups <- if (is.null(group_column)) "__single__" else unique(as.character(daily[[group_column]]))
  rows <- lapply(groups, function(g) {
    x <- if (is.null(group_column)) daily else daily[as.character(daily[[group_column]]) == g, , drop = FALSE]
    x$date <- as.Date(x$date)
    valid_numeric <- is.finite(x$eto_mm_day) & x$eto_mm_day >= 0 &
      is.finite(x$precip_mm_day) & x$precip_mm_day >= 0
    valid_hours <- is.finite(x$hours) & x$hours == 24L
    valid_complete <- if ("complete_day" %in% names(x)) (!is.na(x$complete_day) & x$complete_day) else rep(TRUE, nrow(x))
    valid <- valid_numeric & valid_hours & valid_complete
    got <- sort(unique(x$date[valid]))
    all_dates <- sort(unique(x$date))
    missing <- setdiff(expected, got)
    extra <- setdiff(all_dates, expected)
    bad_hours <- sort(unique(x$date[!valid_hours & x$date %in% expected]))
    bad_values <- sort(unique(x$date[!valid_numeric & x$date %in% expected]))
    incomplete_days <- sort(unique(x$date[!valid_complete & x$date %in% expected]))
    data.frame(
      group = g,
      expected_days = length(expected),
      received_complete_days = sum(got %in% expected),
      missing_or_incomplete_days = paste(as.character(missing), collapse = ";"),
      incomplete_hour_count_days = paste(as.character(bad_hours), collapse = ";"),
      nonfinite_or_invalid_value_days = paste(as.character(bad_values), collapse = ";"),
      incomplete_day_flag_dates = paste(as.character(incomplete_days), collapse = ";"),
      extra_days = paste(as.character(extra), collapse = ";"),
      complete = length(missing) == 0L,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

select_member_scenarios_by_irrigation <- function(member_daily, member_plan_summary,
                                                  validation_start, validation_end) {
  daily <- as.data.frame(member_daily, stringsAsFactors = FALSE)
  plans <- as.data.frame(member_plan_summary, stringsAsFactors = FALSE)
  validation_start <- as.Date(validation_start)
  validation_end <- as.Date(validation_end)
  required_cols <- c("weather_scenario", "required_volume_m3")
  if (!all(required_cols %in% names(plans))) {
    stop("Member plan summary is missing: ", paste(setdiff(required_cols, names(plans)), collapse = ", "), call. = FALSE)
  }
  plans$member_id <- as.character(plans$weather_scenario)
  plans <- plans[is.finite(plans$required_volume_m3), , drop = FALSE]
  plans <- plans[order(plans$required_volume_m3, plans$member_id), , drop = FALSE]
  n <- nrow(plans)
  if (n < 3L) stop("At least three complete member plans are required for scenario selection.", call. = FALSE)
  index_for_prob <- function(prob) max(1L, min(n, as.integer(round(1 + (n - 1) * prob))))
  idx <- c(lower = index_for_prob(0.10), central = index_for_prob(0.50), upper = index_for_prob(0.90))
  selected_ids <- setNames(plans$member_id[idx], names(idx))
  rows <- lapply(names(selected_ids), function(sc) {
    id <- selected_ids[[sc]]
    y <- daily[daily$member_id == id & daily$date >= validation_start & daily$date <= validation_end,
               c("date", "eto_mm_day", "precip_mm_day"), drop = FALSE]
    if (!nrow(y)) stop("Selected ensemble member has no daily data: ", id, call. = FALSE)
    y$weather_scenario <- sc
    y$selected_member_id <- id
    y$forecast_source <- "ECMWF_IFS_0.25_ENSEMBLE_VIA_OPEN_METEO"
    y[, c("weather_scenario", "date", "eto_mm_day", "precip_mm_day", "selected_member_id", "forecast_source")]
  })
  selected_daily <- do.call(rbind, rows)
  row.names(selected_daily) <- NULL
  selection <- data.frame(
    weather_scenario = names(selected_ids),
    selected_member_id = unname(selected_ids),
    member_rank = unname(idx),
    member_count = n,
    rank_probability = c(0.10, 0.50, 0.90),
    required_volume_m3_at_selection = plans$required_volume_m3[idx],
    stringsAsFactors = FALSE
  )
  distribution <- data.frame(
    member_count = n,
    minimum_required_volume_m3 = min(plans$required_volume_m3),
    rank10_required_volume_m3 = plans$required_volume_m3[idx[["lower"]]],
    median_rank_required_volume_m3 = plans$required_volume_m3[idx[["central"]]],
    rank90_required_volume_m3 = plans$required_volume_m3[idx[["upper"]]],
    maximum_required_volume_m3 = max(plans$required_volume_m3),
    stringsAsFactors = FALSE
  )
  list(selected_daily = selected_daily, selection = selection,
       member_plan_summary = plans, distribution = distribution)
}

forecast_request_urls <- function(config) {
  lat <- as.numeric(config$latitude); lon <- as.numeric(config$longitude)
  elev <- as.numeric(config$elevation_m); tz <- as.character(config$timezone)
  days <- as.integer(config$forecast_days)
  det <- build_query_url(as.character(config$deterministic_endpoint), list(
    latitude = lat, longitude = lon, elevation = elev,
    hourly = "et0_fao_evapotranspiration,precipitation",
    models = as.character(config$deterministic_model),
    forecast_days = days, timezone = tz, timeformat = "iso8601"
  ))
  ens <- build_query_url(as.character(config$ensemble_endpoint), list(
    latitude = lat, longitude = lon, elevation = elev,
    hourly = "et0_fao_evapotranspiration,precipitation",
    models = as.character(config$ensemble_model),
    forecast_days = days, timezone = tz, timeformat = "iso8601"
  ))
  list(deterministic = det, ensemble = ens)
}

forecast_run_id <- function() {
  paste0("run_", format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ"))
}

create_forecast_figures <- function(selected_daily, selection, scenario_summary, member_plan_summary, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  f1 <- file.path(out_dir, "Figure_archived_forecast_weather_600dpi.png")
  png(f1, width = 8, height = 7, units = "in", res = 600)
  old <- par(no.readonly = TRUE); on.exit({par(old); dev.off()}, add = TRUE)
  par(mfrow = c(2,1), mar = c(4,4.5,2.5,1))
  scenarios <- c("lower", "central", "upper")
  ltys <- c(2,1,3)
  cols <- c(3,1,2)
  dates <- sort(unique(as.Date(selected_daily$date)))
  yr <- range(selected_daily$eto_mm_day)
  plot(dates, rep(NA_real_, length(dates)), type = "n", ylim = c(0, max(yr)*1.15),
       xlab = "Date", ylab = expression(ET[0]~"(mm/day)"), main = "Archived ECMWF ensemble member scenarios")
  for (i in seq_along(scenarios)) {
    y <- selected_daily[selected_daily$weather_scenario == scenarios[[i]], , drop = FALSE]
    lines(as.Date(y$date), y$eto_mm_day, lty = ltys[[i]], col = cols[[i]], lwd = 2)
  }
  legend("topright", legend = scenarios, lty = ltys, col = cols, lwd = 2, bty = "n")
  maxp <- max(selected_daily$precip_mm_day)
  plot(dates, rep(NA_real_, length(dates)), type = "n", ylim = c(0, max(1, maxp)*1.18),
       xlab = "Date", ylab = "Precipitation (mm/day)")
  for (i in seq_along(scenarios)) {
    y <- selected_daily[selected_daily$weather_scenario == scenarios[[i]], , drop = FALSE]
    lines(as.Date(y$date), y$precip_mm_day, lty = ltys[[i]], col = cols[[i]], lwd = 2)
  }
  dev.off(); on.exit(NULL, add = FALSE)

  f2 <- file.path(out_dir, "Figure_real_forecast_plan_range_600dpi.png")
  png(f2, width = 8, height = 5.5, units = "in", res = 600)
  req <- scenario_summary$required_volume_m3
  del <- scenario_summary$deliverable_volume_m3
  mat <- rbind(req, del)
  bp <- barplot(mat, beside = TRUE, names.arg = scenario_summary$weather_scenario,
                ylab = expression("Water volume (m"^3*")"), ylim = c(0, max(req)*1.28),
                legend.text = c("Required", "Deliverable"), args.legend = list(x = "topleft", bty = "n"))
  text(bp[1,], req, labels = sprintf("%.1f", req), pos = 3, cex = 0.8)
  text(bp[2,], del, labels = sprintf("%.1f", del), pos = 3, cex = 0.8)
  if (any(scenario_summary$unmet_volume_m3 > 0)) {
    idx <- which(scenario_summary$unmet_volume_m3 > 0)
    text(colMeans(bp)[idx], req[idx]*1.15,
         labels = paste0("Deficit ", sprintf("%.1f", scenario_summary$unmet_volume_m3[idx]), " m3"), cex = 0.75)
  }
  dev.off()

  f3 <- file.path(out_dir, "Figure_ensemble_member_irrigation_distribution_600dpi.png")
  png(f3, width = 8, height = 5.5, units = "in", res = 600)
  values <- sort(as.numeric(member_plan_summary$required_volume_m3))
  ylim_top <- max(1, max(values, na.rm = TRUE)) * 1.20
  plot(seq_along(values), values, type = "h", lwd = 1.5,
       xlab = "Ranked complete ECMWF ensemble member",
       ylab = expression("Required irrigation volume (m"^3*")"),
       ylim = c(0, ylim_top), main = "Turn-constrained irrigation requirement across ensemble members")
  points(seq_along(values), values, pch = 16, cex = 0.55)
  selected_ranks <- as.integer(selection$member_rank)
  selected_values <- as.numeric(selection$required_volume_m3_at_selection)
  points(selected_ranks, selected_values, pch = 19, cex = 1.1)
  text(selected_ranks, selected_values,
       labels = paste0(selection$weather_scenario, "\n", sprintf("%.1f m3", selected_values)),
       pos = 3, cex = 0.75)
  dev.off()
  c(f1, f2, f3)
}

write_forecast_report <- function(language, config, run_receipts, selection, scenario_summary,
                                  envelope, evidence_level, run_id, raw_archive_dir, out_dir,
                                  figure_paths = character(0)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  es <- identical(language, "es")
  title <- if (es) "RiegoAzuay DSS - Demostración con pronóstico ECMWF archivado" else "RiegoAzuay DSS - Archived ECMWF forecast demonstration"
  disclaimer <- if (es) {
    "Este informe usa un pronóstico meteorológico real archivado, pero la parcela, el suelo, la humedad inicial, el caudal y el turno corresponden a un caso de demostración. No es una recomendación real de riego."
  } else {
    "This report uses an archived real weather forecast, but the parcel, soil, initial water status, flow and turn are demonstration inputs. It is not a real irrigation recommendation."
  }
  scen <- scenario_summary[, c("weather_scenario","required_volume_m3","deliverable_volume_m3","unmet_volume_m3","required_hours","hours_available","covers_to_next_turn_after_delivery"), drop = FALSE]
  scenario_labels_es <- c(lower = "Inferior (rango 10)", central = "Central (rango 50)", upper = "Superior (rango 90)")
  scenario_labels_en <- c(lower = "Lower (rank 10)", central = "Central (rank 50)", upper = "Upper (rank 90)")
  scen$weather_scenario <- unname((if (es) scenario_labels_es else scenario_labels_en)[as.character(scen$weather_scenario)])
  if (es) {
    names(scen) <- c("Escenario","Agua requerida (m³)","Agua aplicable (m³)","Déficit (m³)","Horas requeridas","Horas disponibles","Cubre hasta próximo turno")
  } else {
    names(scen) <- c("Scenario","Required water (m³)","Deliverable water (m³)","Deficit (m³)","Required hours","Available hours","Covers to next turn")
  }
  sel <- selection[, c("weather_scenario","selected_member_id","member_count","rank_probability","required_volume_m3_at_selection"), drop = FALSE]
  sel$weather_scenario <- unname((if (es) scenario_labels_es else scenario_labels_en)[as.character(sel$weather_scenario)])
  if (es) names(sel) <- c("Escenario","Miembro seleccionado","Miembros disponibles","Cuantil por rango","Volumen requerido del miembro (m³)")
  else names(sel) <- c("Scenario","Selected member","Available members","Rank quantile","Member required volume (m³)")
  central <- scenario_summary[scenario_summary$weather_scenario == "central", , drop = FALSE]
  summary_html <- if (es) paste0(
    "<p><strong>Plan central provisional:</strong> ", sprintf("%.1f", central$required_volume_m3), " m³; ",
    sprintf("%.2f", central$required_hours), " h con el caudal de demostración.</p>",
    "<p><strong>Rango operativo por rango 10–90 del conjunto ECMWF:</strong> ", sprintf("%.1f", envelope$minimum_required_volume_m3), "-", sprintf("%.1f", envelope$maximum_required_volume_m3), " m³.</p>",
    "<p><strong>Nivel de evidencia:</strong> ", evidence_level, ".</p>"
  ) else paste0(
    "<p><strong>Central provisional plan:</strong> ", sprintf("%.1f", central$required_volume_m3), " m³; ",
    sprintf("%.2f", central$required_hours), " h using the demonstration flow.</p>",
    "<p><strong>ECMWF rank-10 to rank-90 operational range:</strong> ", sprintf("%.1f", envelope$minimum_required_volume_m3), "-", sprintf("%.1f", envelope$maximum_required_volume_m3), " m³.</p>",
    "<p><strong>Evidence level:</strong> ", evidence_level, ".</p>"
  )
  table_html <- function(df) {
    hdr <- paste0("<th>", html_escape(names(df)), "</th>", collapse = "")
    rows <- apply(df, 1, function(r) paste0("<tr>", paste0("<td>", html_escape(r), "</td>", collapse = ""), "</tr>"))
    paste0("<table><thead><tr>", hdr, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")
  }
  image_html <- ""
  if (length(figure_paths)) {
    existing <- figure_paths[file.exists(figure_paths)]
    if (length(existing)) {
      image_titles <- if (es) {
        c("Trayectorias meteorológicas seleccionadas", "Plan de riego por rango", "Distribución de miembros del conjunto")
      } else {
        c("Selected weather trajectories", "Rank-based irrigation plan", "Ensemble-member distribution")
      }
      image_html <- paste(vapply(seq_along(existing), function(i) {
        uri <- base64enc::dataURI(file = existing[[i]], mime = "image/png")
        paste0("<h2>", html_escape(image_titles[[min(i, length(image_titles))]]), "</h2>",
               "<img class='figure' src='", uri, "' alt='", html_escape(image_titles[[min(i, length(image_titles))]]), "'>")
      }, character(1)), collapse = "")
    }
  }
  file <- file.path(out_dir, if (es) "ARCHIVED_FORECAST_PLAN_ES.html" else "ARCHIVED_FORECAST_PLAN_EN.html")
  html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>", html_escape(title), "</title>",
    "<style>body{font-family:Arial,sans-serif;max-width:1100px;margin:30px auto;line-height:1.45;color:#17365D}",
    "h1,h2{color:#17365D}.card{padding:16px;border:1px solid #ccd6df;border-radius:10px;margin:12px 0;background:#f7fafc}",
    ".warn{padding:14px;border-left:5px solid #d39e00;background:#fff4cc;margin:12px 0}",
    "table{border-collapse:collapse;width:100%;font-size:13px}th,td{border:1px solid #d7dee5;padding:7px;text-align:left}th{background:#17365D;color:white}.figure{max-width:100%;height:auto;border:1px solid #d7dee5}</style></head><body>",
    "<h1>", html_escape(title), "</h1><div class='warn'>", html_escape(disclaimer), "</div>",
    "<div class='card'>", summary_html, "</div>",
    "<h2>", if (es) "Escenarios operativos" else "Operational scenarios", "</h2>", table_html(scen),
    "<h2>", if (es) "Selección por rango del conjunto" else "Ensemble rank selection", "</h2>", table_html(sel),
    image_html,
    "<h2>", if (es) "Trazabilidad" else "Traceability", "</h2>",
    "<p>Run ID: ", html_escape(run_id), "</p><p>", html_escape(config$attribution), "</p>",
    "<p>", if (es) "Política ET0: los valores horarios negativos plausibles se conservan en la suma diaria; solo una suma diaria negativa se fija en cero para la programación." else "ET0 policy: plausible negative hourly values are preserved in the daily sum; only a negative daily sum is floored at zero for scheduling.", "</p>",
    "<p>", if (es) "Los archivos JSON originales, los diagnósticos de signo y los recibos SHA-256 se incluyen en el paquete reproducible." else "Original JSON payloads, sign diagnostics and SHA-256 receipts are included in the reproducibility bundle.", "</p>",
    "</body></html>"
  )
  writeLines(html, file, useBytes = TRUE)
  file
}

build_forecast_bundle <- function(run_dir, bundle_path) {
  if (!dir.exists(run_dir)) stop("Forecast run directory not found: ", run_dir, call. = FALSE)
  run_dir_abs <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
  bundle_dir <- dirname(bundle_path)
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  bundle_dir_abs <- normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
  bundle_abs <- file.path(bundle_dir_abs, basename(bundle_path))

  manifest_path <- file.path(run_dir_abs, "BUNDLE_MANIFEST_SHA256.csv")
  if (file.exists(manifest_path)) unlink(manifest_path, force = TRUE)
  files <- list.files(run_dir_abs, recursive = TRUE, full.names = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  rel <- substring(files, nchar(run_dir_abs) + 2L)
  manifest <- data.frame(
    relative_path = gsub("\\\\", "/", rel),
    size_bytes = file.info(files)$size,
    sha256 = vapply(files, function(p) digest::digest(p, algo = "sha256", file = TRUE), character(1)),
    stringsAsFactors = FALSE
  )
  write_csv_utf8(manifest, manifest_path)

  files_rel <- list.files(run_dir_abs, recursive = TRUE, all.files = TRUE, full.names = FALSE,
                          include.dirs = FALSE, no.. = TRUE)
  if (!length(files_rel)) stop("Forecast run directory contains no files to archive.", call. = FALSE)
  missing_stage <- files_rel[!file.exists(file.path(run_dir_abs, files_rel))]
  if (length(missing_stage)) {
    stop("Forecast bundle source files are missing: ", paste(missing_stage, collapse = ", "), call. = FALSE)
  }

  temp_bundle <- tempfile(
    pattern = paste0(".", tools::file_path_sans_ext(basename(bundle_abs)), "_"),
    tmpdir = bundle_dir_abs,
    fileext = ".zip"
  )
  cleanup_temp <- TRUE
  on.exit({
    if (cleanup_temp && file.exists(temp_bundle)) unlink(temp_bundle, force = TRUE)
  }, add = TRUE)

  zip::zip(zipfile = temp_bundle, files = files_rel, recurse = FALSE,
           include_directories = FALSE, root = run_dir_abs, mode = "mirror")
  if (!file.exists(temp_bundle)) stop("Temporary forecast bundle was not created.", call. = FALSE)
  entries <- gsub("\\\\", "/", utils::unzip(temp_bundle, list = TRUE)$Name)
  missing <- setdiff(gsub("\\\\", "/", files_rel), entries)
  if (length(missing)) stop("Forecast bundle is incomplete: ", paste(missing, collapse = ", "), call. = FALSE)

  if (file.exists(bundle_abs)) unlink(bundle_abs, force = TRUE)
  moved <- file.rename(temp_bundle, bundle_abs)
  if (!isTRUE(moved)) {
    copied <- file.copy(temp_bundle, bundle_abs, overwrite = TRUE)
    if (!isTRUE(copied)) stop("The verified forecast bundle could not be moved to: ", bundle_abs, call. = FALSE)
    unlink(temp_bundle, force = TRUE)
  }
  cleanup_temp <- FALSE
  if (!file.exists(bundle_abs)) stop("The final forecast bundle was not created.", call. = FALSE)
  bundle_abs
}

forecast_bundle_relative_path_smoke_test <- function() {
  run_root <- tempfile("forecast_bundle_run_")
  dir.create(file.path(run_root, "results"), recursive = TRUE, showWarnings = FALSE)
  writeLines("forecast bundle smoke test", file.path(run_root, "results", "probe.txt"), useBytes = TRUE)
  relative_bundle <- file.path("outputs", "diagnostics", "forecast_bundle_relative_path_smoke_test.zip")
  dir.create(dirname(relative_bundle), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(relative_bundle)) unlink(relative_bundle, force = TRUE)
  result <- try(build_forecast_bundle(run_root, relative_bundle), silent = TRUE)
  if (inherits(result, "try-error")) {
    unlink(run_root, recursive = TRUE, force = TRUE)
    return(list(created = FALSE, readable = FALSE, nested_path_preserved = FALSE,
                manifest_present = FALSE, manifest_self_excluded = FALSE, absolute_target = FALSE,
                error = as.character(result), entries = character(0)))
  }
  entries <- gsub("\\\\", "/", utils::unzip(result, list = TRUE)$Name)
  out <- list(
    created = file.exists(result),
    readable = length(entries) > 0L,
    nested_path_preserved = "results/probe.txt" %in% entries,
    manifest_present = "BUNDLE_MANIFEST_SHA256.csv" %in% entries,
    manifest_self_excluded = {
      manifest_file <- file.path(run_root, "BUNDLE_MANIFEST_SHA256.csv")
      manifest_rows <- if (file.exists(manifest_file)) utils::read.csv(manifest_file, stringsAsFactors = FALSE) else data.frame()
      nrow(manifest_rows) > 0L && !"BUNDLE_MANIFEST_SHA256.csv" %in% as.character(manifest_rows$relative_path)
    },
    absolute_target = grepl("^([A-Za-z]:/|/)", gsub("\\\\", "/", result)),
    error = "",
    entries = entries
  )
  if (file.exists(result)) unlink(result, force = TRUE)
  unlink(run_root, recursive = TRUE, force = TRUE)
  out
}

copy_directory_tree <- function(source_dir, target_dir) {
  if (!dir.exists(source_dir)) stop("Source directory not found: ", source_dir, call. = FALSE)
  if (dir.exists(target_dir)) unlink(target_dir, recursive = TRUE, force = TRUE)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(source_dir, recursive = TRUE, all.files = TRUE, full.names = TRUE,
                      include.dirs = FALSE, no.. = TRUE)
  rel <- substring(files, nchar(source_dir) + 2L)
  for (i in seq_along(files)) {
    target <- file.path(target_dir, rel[[i]])
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(files[[i]], target, overwrite = TRUE, copy.date = TRUE)) {
      stop("Failed to copy prior forecast-run file: ", files[[i]], call. = FALSE)
    }
  }
  invisible(target_dir)
}

find_previous_complete_forecast_run <- function(search_root = dirname(normalizePath(getwd(), winslash = "/", mustWork = TRUE))) {
  explicit <- Sys.getenv("FORECAST_COMPLETE_RUN_DIR", unset = "")
  run_dirs <- character(0)
  if (nzchar(explicit) && dir.exists(explicit)) {
    run_dirs <- normalizePath(explicit, winslash = "/", mustWork = TRUE)
  } else {
    dirs <- list.dirs(search_root, recursive = TRUE, full.names = TRUE)
    package_dirs <- dirs[grepl("Article4_DailyBalance_ForecastArchive_v2\\.1\\.[0-3]", basename(dirs))]
    current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    package_dirs <- setdiff(normalizePath(package_dirs, winslash = "/", mustWork = TRUE), current)
    for (pkg in unique(package_dirs)) {
      runs_base <- file.path(pkg, "outputs", "forecast_runs")
      if (dir.exists(runs_base)) {
        run_dirs <- c(run_dirs, list.dirs(runs_base, recursive = FALSE, full.names = TRUE))
      }
    }
  }
  if (!length(run_dirs)) return(NULL)
  required_rel <- c(
    file.path("raw", "openmeteo_ecmwf_ifs_deterministic.json"),
    file.path("raw", "openmeteo_ecmwf_ifs025_ensemble.json"),
    file.path("results", "forecast_required_horizon_status.csv"),
    file.path("results", "ensemble_member_plan_summary.csv"),
    file.path("results", "ensemble_selected_members.csv"),
    file.path("results", "weekly_plan_ensemble_scenarios.csv"),
    file.path("results", "archived_forecast_demo_summary.csv"),
    file.path("reports", "ARCHIVED_FORECAST_PLAN_ES.html"),
    file.path("reports", "ARCHIVED_FORECAST_PLAN_EN.html"),
    file.path("reproducibility", "run_metadata.csv")
  )
  candidates <- lapply(unique(run_dirs), function(run_dir) {
    required <- file.path(run_dir, required_rel)
    if (!all(file.exists(required))) return(NULL)
    list(run_dir = run_dir, run_id = basename(run_dir), mtime = max(file.info(required)$mtime))
  })
  candidates <- Filter(Negate(is.null), candidates)
  if (!length(candidates)) return(NULL)
  candidates[[which.max(vapply(candidates, function(x) as.numeric(x$mtime), numeric(1)))]]
}

read_run_metadata_value <- function(run_dir, key, default = "") {
  path <- file.path(run_dir, "reproducibility", "run_metadata.csv")
  if (!file.exists(path)) return(default)
  x <- try(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
  if (inherits(x, "try-error") || !all(c("key", "value") %in% names(x))) return(default)
  idx <- which(as.character(x$key) == key)
  if (!length(idx)) return(default)
  value <- as.character(x$value[[idx[[1]]]])
  if (is.na(value)) default else value
}

recover_complete_forecast_run_bundle <- function(candidate) {
  if (is.null(candidate) || !dir.exists(candidate$run_dir)) {
    stop("No complete prior forecast run is available for bundle repair.", call. = FALSE)
  }
  destination_run_id <- paste0(candidate$run_id, "_bundle_repaired_v214")
  destination_run_dir <- file.path("outputs", "forecast_runs", destination_run_id)
  copy_directory_tree(candidate$run_dir, destination_run_dir)
  receipt <- data.frame(
    source_run_dir = normalizePath(candidate$run_dir, winslash = "/", mustWork = TRUE),
    source_run_id = candidate$run_id,
    source_scientific_package_version = read_run_metadata_value(candidate$run_dir, "package_version", "2.1.3"),
    packaging_patch_version = package_version_string(),
    repair_timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    repair_reason = "The scientific run completed through report generation; the v2.1.3 relative ZIP target could not be opened after zip changed its working directory to the run root.",
    scientific_results_recomputed = FALSE,
    raw_forecast_download_repeated = FALSE,
    stringsAsFactors = FALSE
  )
  write_csv_utf8(receipt, file.path(destination_run_dir, "reproducibility", "bundle_repair_receipt.csv"))
  bundle_path <- file.path(
    "outputs", "bundles",
    paste0("Article4_DailyBalance_ForecastArchive_v2.1.4_", candidate$run_id, "_recovered.zip")
  )
  bundle <- build_forecast_bundle(destination_run_dir, bundle_path)
  list(bundle = bundle, destination_run_dir = destination_run_dir, receipt = receipt)
}
