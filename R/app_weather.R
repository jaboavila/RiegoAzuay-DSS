audit_retained_member_horizons <- function(member_daily, expected_dates, minimum_members = 1L) {
  x <- as.data.frame(member_daily, stringsAsFactors = FALSE)
  expected <- as.character(as.Date(expected_dates))
  expected <- sort(unique(expected[!is.na(expected)]))
  required <- c("member_id", "date")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols)) {
    stop("Member-horizon audit is missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!nrow(x) || !length(expected)) {
    diagnostics <- data.frame(
      member_id = character(0), row_count = integer(0), unique_date_count = integer(0),
      duplicate_date_count = integer(0), first_date = character(0), last_date = character(0),
      missing_dates = character(0), extra_dates = character(0), exact_horizon = logical(0),
      stringsAsFactors = FALSE
    )
    return(list(ok = FALSE, diagnostics = diagnostics, retained_member_count = 0L,
                expected_dates = expected))
  }
  x$member_id <- as.character(x$member_id)
  x$date_chr <- as.character(as.Date(x$date))
  ids <- sort(unique(x$member_id[nzchar(x$member_id)]))
  rows <- lapply(ids, function(id) {
    dates <- x$date_chr[x$member_id == id]
    dates <- dates[!is.na(dates) & nzchar(dates)]
    unique_dates <- sort(unique(dates))
    missing_dates <- setdiff(expected, unique_dates)
    extra_dates <- setdiff(unique_dates, expected)
    duplicate_count <- length(dates) - length(unique_dates)
    exact <- length(dates) == length(expected) &&
      length(unique_dates) == length(expected) &&
      duplicate_count == 0L &&
      !length(missing_dates) &&
      !length(extra_dates) &&
      all(unique_dates == expected)
    data.frame(
      member_id = id,
      row_count = length(dates),
      unique_date_count = length(unique_dates),
      duplicate_date_count = duplicate_count,
      first_date = if (length(unique_dates)) unique_dates[[1]] else "",
      last_date = if (length(unique_dates)) unique_dates[[length(unique_dates)]] else "",
      missing_dates = paste(missing_dates, collapse = ";"),
      extra_dates = paste(extra_dates, collapse = ";"),
      exact_horizon = exact,
      stringsAsFactors = FALSE
    )
  })
  diagnostics <- if (length(rows)) do.call(rbind, rows) else data.frame(
    member_id = character(0), row_count = integer(0), unique_date_count = integer(0),
    duplicate_date_count = integer(0), first_date = character(0), last_date = character(0),
    missing_dates = character(0), extra_dates = character(0), exact_horizon = logical(0),
    stringsAsFactors = FALSE
  )
  ok <- nrow(diagnostics) >= as.integer(minimum_members) &&
    nrow(diagnostics) > 0L && all(!is.na(diagnostics$exact_horizon) & diagnostics$exact_horizon)
  list(ok = ok, diagnostics = diagnostics, retained_member_count = nrow(diagnostics),
       expected_dates = expected)
}

prepare_live_forecast_ensemble <- function(latitude, longitude, elevation_m,
                                           start_date, end_date, app_config,
                                           run_dir = tempfile("riegoazuay_forecast_")) {
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || end_date < start_date) {
    stop("Invalid live-forecast horizon.", call. = FALSE)
  }
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  raw_dir <- file.path(run_dir, "raw")
  receipt_dir <- file.path(run_dir, "receipts")
  diagnostic_dir <- file.path(run_dir, "diagnostics")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(receipt_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)

  cfg <- list(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    elevation_m = if (is.finite(as.numeric(elevation_m))) as.numeric(elevation_m) else NA_real_,
    timezone = as.character(app_config$timezone),
    deterministic_endpoint = as.character(app_config$forecast_deterministic_endpoint),
    deterministic_model = as.character(app_config$forecast_deterministic_model),
    ensemble_endpoint = as.character(app_config$forecast_ensemble_endpoint),
    ensemble_model = as.character(app_config$forecast_ensemble_model),
    forecast_days = as.integer(app_config$forecast_days)
  )
  urls <- forecast_request_urls(cfg)
  ensemble_raw <- file.path(raw_dir, "openmeteo_ecmwf_ensemble.json")
  ensemble_receipt <- file.path(receipt_dir, "ensemble_request_receipt.csv")
  ensemble_response <- archive_http_json(
    urls$ensemble, ensemble_raw, ensemble_receipt,
    "ECMWF_IFS025_ENSEMBLE_VIA_OPEN_METEO", timeout_seconds = 180
  )

  ensemble_hourly <- parse_openmeteo_ensemble(
    ensemble_response$json,
    diagnostic_path = file.path(diagnostic_dir, "ensemble_value_diagnostics.csv"),
    required_start_date = start_date,
    required_end_date = end_date,
    fatal_mode = "mark"
  )
  ensemble_daily <- aggregate_hourly_daily(ensemble_hourly, member_column = "member_id")
  coverage_all <- validate_daily_coverage(ensemble_daily, start_date, end_date, group_column = "member_id")
  complete_members <- as.character(coverage_all$group[!is.na(coverage_all$complete) & coverage_all$complete])
  incomplete_members <- as.character(coverage_all$group[is.na(coverage_all$complete) | !coverage_all$complete])
  coverage_retained <- coverage_all[coverage_all$group %in% complete_members, , drop = FALSE]
  coverage_excluded <- coverage_all[coverage_all$group %in% incomplete_members, , drop = FALSE]
  minimum_members <- as.integer(app_config$minimum_ensemble_members)
  write_csv_utf8(coverage_all, file.path(diagnostic_dir, "ensemble_daily_coverage.csv"))
  write_csv_utf8(coverage_all, file.path(diagnostic_dir, "ensemble_daily_coverage_all_members.csv"))
  write_csv_utf8(coverage_retained, file.path(diagnostic_dir, "ensemble_daily_coverage_retained_members.csv"))
  write_csv_utf8(coverage_excluded, file.path(diagnostic_dir, "ensemble_daily_coverage_excluded_members.csv"))
  if (length(complete_members) < minimum_members) {
    stop(
      "Live ensemble gateway retained ", length(complete_members),
      " complete members; at least ", minimum_members, " are required.",
      call. = FALSE
    )
  }
  ensemble_daily <- ensemble_daily[
    ensemble_daily$member_id %in% complete_members &
      ensemble_daily$date >= start_date & ensemble_daily$date <= end_date,
    , drop = FALSE
  ]
  ensemble_daily$weather_scenario <- as.character(ensemble_daily$member_id)
  ensemble_daily$forecast_source <- "ECMWF_IFS_0.25_ENSEMBLE_VIA_OPEN_METEO"
  write_csv_utf8(ensemble_daily, file.path(run_dir, "ensemble_daily_complete_members.csv"))

  deterministic <- NULL
  deterministic_warning <- ""
  deterministic_raw <- file.path(raw_dir, "openmeteo_ecmwf_deterministic.json")
  deterministic_receipt <- file.path(receipt_dir, "deterministic_request_receipt.csv")
  try_result <- try({
    det_response <- archive_http_json(
      urls$deterministic, deterministic_raw, deterministic_receipt,
      "ECMWF_IFS_HRES_VIA_OPEN_METEO", timeout_seconds = 180
    )
    det_hourly <- parse_openmeteo_deterministic(
      det_response$json,
      diagnostic_path = file.path(diagnostic_dir, "deterministic_value_diagnostics.csv"),
      required_start_date = start_date,
      required_end_date = end_date,
      fatal_mode = "mark"
    )
    det_daily <- aggregate_hourly_daily(det_hourly)
    det_coverage <- validate_daily_coverage(det_daily, start_date, end_date)
    write_csv_utf8(det_daily, file.path(run_dir, "deterministic_daily.csv"))
    write_csv_utf8(det_coverage, file.path(diagnostic_dir, "deterministic_daily_coverage.csv"))
    deterministic <- det_daily
  }, silent = TRUE)
  if (inherits(try_result, "try-error")) {
    deterministic_warning <- paste0(
      "Deterministic forecast could not be retained; the primary ensemble workflow remains available. ",
      substr(as.character(try_result), 1, 500)
    )
    writeLines(deterministic_warning, file.path(diagnostic_dir, "deterministic_warning.txt"), useBytes = TRUE)
  }

  list(
    member_daily = ensemble_daily,
    deterministic_daily = deterministic,
    coverage = coverage_retained,
    coverage_all = coverage_all,
    excluded_incomplete_members = incomplete_members,
    complete_member_count = length(complete_members),
    run_dir = run_dir,
    request_urls = urls,
    deterministic_warning = deterministic_warning,
    source_class = "PUBLIC_MODELLED"
  )
}

prepare_weather_input <- function(mode, start_date, end_date, upload_path = NULL,
                                  latitude = NA_real_, longitude = NA_real_, elevation_m = NA_real_,
                                  app_config = NULL, run_dir = NULL) {
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  if (mode == "offline_fixture") {
    x <- build_relative_weather_fixture(start_date, end_date)
    x$weather_scenario <- normalise_weather_scenario_labels(x$weather_scenario)
    return(list(
      selected_weather = x,
      member_daily = NULL,
      source_class = "SYNTHETIC_TEST",
      source_label = "Synthetic demonstration weather data (offline)",
      run_dir = NULL,
      warnings = character(0)
    ))
  }
  if (mode == "archived_fixture") {
    x <- build_shifted_archived_weather(start_date, end_date)
    x$weather_scenario <- normalise_weather_scenario_labels(x$weather_scenario)
    return(list(
      selected_weather = x,
      member_daily = NULL,
      source_class = "PUBLIC_MODELLED",
      source_label = "Saved ECMWF demonstration forecast shifted for reproducible testing",
      run_dir = NULL,
      warnings = "Saved forecast values are not current conditions and are used only for a reproducible software demonstration."
    ))
  }
  if (mode == "manual_csv") {
    x <- read_manual_weather(upload_path, start_date, end_date)
    x$weather_scenario <- normalise_weather_scenario_labels(x$weather_scenario)
    return(list(
      selected_weather = x,
      member_daily = NULL,
      source_class = "USER_DECLARED",
      source_label = "User-supplied or local-station weather CSV",
      run_dir = NULL,
      warnings = character(0),
      upload_sha256 = hash_file_sha256(upload_path)
    ))
  }
  if (mode == "live_openmeteo") {
    if (is.null(app_config)) stop("app_config is required for live weather.", call. = FALSE)
    live <- prepare_live_forecast_ensemble(
      latitude, longitude, elevation_m, start_date, end_date, app_config,
      run_dir = run_dir %||% tempfile("riegoazuay_forecast_")
    )
    return(list(
      selected_weather = NULL,
      member_daily = live$member_daily,
      source_class = "PUBLIC_MODELLED",
      source_label = "Live ECMWF IFS ensemble via Open-Meteo",
      run_dir = live$run_dir,
      warnings = if (nzchar(live$deterministic_warning)) live$deterministic_warning else character(0),
      live = live
    ))
  }
  stop("Unknown weather mode: ", mode, call. = FALSE)
}
