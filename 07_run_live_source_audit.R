source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "app_data.R"), encoding = "UTF-8")
source(file.path("R", "profile_library.R"), encoding = "UTF-8")
source(file.path("R", "layered_soil_balance.R"), encoding = "UTF-8")
source(file.path("R", "forecast_archive.R"), encoding = "UTF-8")
source(file.path("R", "app_weather.R"), encoding = "UTF-8")
source(file.path("R", "soilgrids_gateway.R"), encoding = "UTF-8")
source(file.path("R", "soil_profile_integration.R"), encoding = "UTF-8")
source(file.path("R", "app_soil.R"), encoding = "UTF-8")
source(file.path("R", "app_reporting.R"), encoding = "UTF-8")

cat("\n=== RIEGOAZUAY DSS: LIVE PUBLIC-SOURCE AUDIT ===\n")

app_config <- load_app_config()
version <- package_version_string()
timezone <- as.character(app_config$timezone)
latitude <- as.numeric(app_config$scientific_demo_latitude)
longitude <- as.numeric(app_config$scientific_demo_longitude)
elevation_m <- as.numeric(app_config$scientific_demo_elevation_m)
local_today <- as.Date(format(Sys.time(), tz = timezone, format = "%Y-%m-%d"))
start_date <- local_today
end_date <- local_today + 7L
expected_dates <- seq(start_date, end_date, by = "day")
run_id <- paste0("live_source_audit_", format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ"))
audit_parent <- file.path("outputs", "live_source_audit")
audit_dir <- file.path(audit_parent, run_id)
weather_dir <- file.path(audit_dir, "weather")
soil_dir <- file.path(audit_dir, "soil")
dir.create(weather_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(soil_dir, recursive = TRUE, showWarnings = FALSE)

metadata <- data.frame(
  field = c(
    "software", "version", "run_id", "created_utc", "timezone",
    "latitude", "longitude", "elevation_m", "start_date", "end_date",
    "weather_deterministic_endpoint", "weather_ensemble_endpoint",
    "soilgrids_wcs_endpoint", "soilgrids_webdav_endpoint",
    "interpretation_boundary"
  ),
  value = c(
    "RiegoAzuay DSS", version, run_id,
    format(Sys.time(), tz = "UTC", usetz = TRUE), timezone,
    format(latitude, digits = 12), format(longitude, digits = 12), format(elevation_m, digits = 12),
    as.character(start_date), as.character(end_date),
    as.character(app_config$forecast_deterministic_endpoint),
    as.character(app_config$forecast_ensemble_endpoint),
    as.character(app_config$soilgrids_wcs_base_url),
    as.character(app_config$soilgrids_webdav_base_url),
    "This gateway audit verifies acquisition, coverage, payload validity, provenance receipts, and profile construction. It does not validate parcel-scale weather or soil accuracy."
  ),
  stringsAsFactors = FALSE
)
write_csv_utf8(metadata, file.path(audit_dir, "LIVE_SOURCE_AUDIT_METADATA.csv"))
writeLines(capture.output(sessionInfo()), file.path(audit_dir, "sessionInfo.txt"), useBytes = TRUE)

checks <- list()
add_check <- function(id, description, pass, observed, expected) {
  checks[[length(checks) + 1L]] <<- new_test_row(id, description, isTRUE(pass), observed, expected)
}

add_check("LIVE001", "Audit package is the v1.0.0 live-source gate", identical(version, "1.0.0"), version, "1.0.0")
add_check(
  "LIVE002", "Scientific demonstration coordinate is finite and geographically plausible",
  all(is.finite(c(latitude, longitude, elevation_m))) && latitude >= -90 && latitude <= 90 && longitude >= -180 && longitude <= 180,
  paste(latitude, longitude, elevation_m, sep = " / "), "finite WGS84 coordinate and elevation"
)
add_check("LIVE003", "Audit horizon contains eight consecutive calendar days", length(expected_dates) == 8L && all(diff(expected_dates) == 1), paste(expected_dates, collapse = " | "), "8 consecutive days")

# -------------------------------------------------------------------------
# Open-Meteo / ECMWF live audit
# -------------------------------------------------------------------------
det_endpoint <- as.character(app_config$forecast_deterministic_endpoint)
ens_endpoint <- as.character(app_config$forecast_ensemble_endpoint)
add_check("WTH001", "Configured deterministic route is an HTTPS Open-Meteo endpoint", grepl("^https://", det_endpoint) && grepl("open-meteo.com", det_endpoint, fixed = TRUE), det_endpoint, "HTTPS Open-Meteo endpoint")
add_check("WTH002", "Configured ensemble route is an HTTPS Open-Meteo endpoint", grepl("^https://", ens_endpoint) && grepl("open-meteo.com", ens_endpoint, fixed = TRUE), ens_endpoint, "HTTPS Open-Meteo endpoint")

weather_warnings <- character(0)
weather_outcome <- withCallingHandlers(
  tryCatch(
    list(
      ok = TRUE,
      value = prepare_live_forecast_ensemble(
        latitude = latitude,
        longitude = longitude,
        elevation_m = elevation_m,
        start_date = start_date,
        end_date = end_date,
        app_config = app_config,
        run_dir = weather_dir
      ),
      error = ""
    ),
    error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e))
  ),
  warning = function(w) {
    weather_warnings <<- c(weather_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
writeLines(
  if (length(weather_warnings)) unique(weather_warnings) else "No warnings captured.",
  file.path(weather_dir, "LIVE_WEATHER_WARNINGS.txt"),
  useBytes = TRUE
)
if (!weather_outcome$ok) {
  writeLines(weather_outcome$error, file.path(weather_dir, "LIVE_WEATHER_ERROR.txt"), useBytes = TRUE)
}
add_check("WTH003", "Live ECMWF/Open-Meteo workflow completes", weather_outcome$ok, if (weather_outcome$ok) "completed" else weather_outcome$error, "completed")

weather <- weather_outcome$value
ens_raw <- file.path(weather_dir, "raw", "openmeteo_ecmwf_ensemble.json")
ens_receipt_path <- file.path(weather_dir, "receipts", "ensemble_request_receipt.csv")
det_raw <- file.path(weather_dir, "raw", "openmeteo_ecmwf_deterministic.json")
det_receipt_path <- file.path(weather_dir, "receipts", "deterministic_request_receipt.csv")
read_receipt_safe <- function(path) {
  if (!file.exists(path)) return(data.frame())
  tryCatch(as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE), error = function(e) data.frame())
}
ens_receipt <- read_receipt_safe(ens_receipt_path)
det_receipt <- read_receipt_safe(det_receipt_path)
ens_status <- if (nrow(ens_receipt) && "http_status" %in% names(ens_receipt)) suppressWarnings(as.integer(ens_receipt$http_status[[1]])) else NA_integer_
det_status <- if (nrow(det_receipt) && "http_status" %in% names(det_receipt)) suppressWarnings(as.integer(det_receipt$http_status[[1]])) else NA_integer_
ens_hash_recorded <- if (nrow(ens_receipt) && "raw_sha256" %in% names(ens_receipt)) as.character(ens_receipt$raw_sha256[[1]]) else ""
det_hash_recorded <- if (nrow(det_receipt) && "raw_sha256" %in% names(det_receipt)) as.character(det_receipt$raw_sha256[[1]]) else ""

add_check("WTH004", "Ensemble raw JSON is archived and non-empty", file.exists(ens_raw) && is.finite(file.info(ens_raw)$size) && file.info(ens_raw)$size > 0, if (file.exists(ens_raw)) file.info(ens_raw)$size else 0, ">0 bytes")
add_check("WTH005", "Ensemble receipt records a successful HTTP response", nrow(ens_receipt) == 1L && is.finite(ens_status) && ens_status >= 200L && ens_status < 300L, ens_status, "HTTP 2xx")
add_check("WTH006", "Ensemble receipt SHA-256 matches the archived JSON", file.exists(ens_raw) && nzchar(ens_hash_recorded) && identical(tolower(hash_file_sha256(ens_raw)), tolower(ens_hash_recorded)), paste(hash_file_sha256(ens_raw), ens_hash_recorded, sep = " / "), "matching SHA-256")

member_count <- if (!is.null(weather)) as.integer(weather$complete_member_count) else 0L
minimum_members <- as.integer(app_config$minimum_ensemble_members)
add_check("WTH007", "Retained ensemble contains at least the configured minimum complete members", member_count >= minimum_members, member_count, paste0(">=", minimum_members))

ensemble_coverage_ok <- FALSE
ensemble_values_ok <- FALSE
ensemble_dates_ok <- FALSE
excluded_not_retained_ok <- FALSE
member_count_consistent_ok <- FALSE
retained_ids <- character(0)
excluded_ids <- character(0)
retained_coverage <- data.frame()
all_coverage <- data.frame()
horizon_audit <- list(ok = FALSE, diagnostics = data.frame(), retained_member_count = 0L)
if (!is.null(weather)) {
  if (is.data.frame(weather$member_daily) && nrow(weather$member_daily)) {
    retained_ids <- sort(unique(as.character(weather$member_daily$member_id)))
    ensemble_values_ok <- all(is.finite(weather$member_daily$eto_mm_day) & weather$member_daily$eto_mm_day >= 0) &&
      all(is.finite(weather$member_daily$precip_mm_day) & weather$member_daily$precip_mm_day >= 0)
    horizon_audit <- audit_retained_member_horizons(
      weather$member_daily, expected_dates, minimum_members = minimum_members
    )
    ensemble_dates_ok <- isTRUE(horizon_audit$ok)
    write_csv_utf8(
      horizon_audit$diagnostics,
      file.path(weather_dir, "diagnostics", "release_gate_retained_member_horizons.csv")
    )
  }
  if (is.data.frame(weather$coverage)) retained_coverage <- weather$coverage
  if (is.data.frame(weather$coverage_all)) all_coverage <- weather$coverage_all
  excluded_ids <- sort(unique(as.character(weather$excluded_incomplete_members %||% character(0))))
  ensemble_coverage_ok <- nrow(retained_coverage) == length(retained_ids) &&
    nrow(retained_coverage) >= minimum_members &&
    all(!is.na(retained_coverage$complete) & retained_coverage$complete) &&
    setequal(as.character(retained_coverage$group), retained_ids)
  excluded_not_retained_ok <- !length(intersect(excluded_ids, retained_ids)) &&
    (!nrow(all_coverage) || all(excluded_ids %in% as.character(all_coverage$group)))
  member_count_consistent_ok <- member_count == length(retained_ids) &&
    member_count == nrow(retained_coverage) &&
    member_count == as.integer(horizon_audit$retained_member_count)
}
coverage_observed <- if (length(retained_ids)) {
  paste0(
    "retained=", length(retained_ids),
    "; excluded_incomplete=", if (length(excluded_ids)) paste(excluded_ids, collapse = "|") else "none"
  )
} else {
  "no retained members"
}
failed_horizon_rows <- if (is.data.frame(horizon_audit$diagnostics) && nrow(horizon_audit$diagnostics)) {
  horizon_audit$diagnostics[!horizon_audit$diagnostics$exact_horizon, , drop = FALSE]
} else data.frame()
horizon_observed <- if (ensemble_dates_ok) {
  paste0(length(retained_ids), " retained members x ", length(expected_dates), " exact dates")
} else if (nrow(failed_horizon_rows)) {
  paste(
    paste0(
      failed_horizon_rows$member_id,
      "[rows=", failed_horizon_rows$row_count,
      ";duplicates=", failed_horizon_rows$duplicate_date_count,
      ";missing=", failed_horizon_rows$missing_dates,
      ";extra=", failed_horizon_rows$extra_dates, "]"
    ),
    collapse = " | "
  )
} else {
  "no retained-member horizon diagnostics"
}
add_check("WTH008", "All retained ensemble members completely cover the audit horizon", ensemble_coverage_ok, coverage_observed, "all retained members complete; incomplete members may be excluded")
add_check("WTH009", "Ensemble daily ET0 and precipitation values are finite and non-negative", ensemble_values_ok, if (!is.null(weather) && is.data.frame(weather$member_daily)) nrow(weather$member_daily) else 0L, "all finite and >=0")
add_check("WTH010", "Each retained ensemble member contains the exact eight-day horizon", ensemble_dates_ok, horizon_observed, paste(as.character(expected_dates), collapse = " | "))
add_check("WTH016", "Incomplete ensemble members are excluded from the retained daily table", excluded_not_retained_ok, coverage_observed, "no excluded member retained")
add_check("WTH017", "Retained-member counts agree across summary, coverage, and daily horizon diagnostics", member_count_consistent_ok, paste(member_count, length(retained_ids), nrow(retained_coverage), horizon_audit$retained_member_count, sep = " / "), "all counts equal")

deterministic_retained <- !is.null(weather) && is.data.frame(weather$deterministic_daily) && nrow(weather$deterministic_daily) > 0L
add_check("WTH011", "Deterministic ECMWF route is retained alongside the ensemble route", deterministic_retained, if (deterministic_retained) nrow(weather$deterministic_daily) else weather$deterministic_warning %||% "not retained", "deterministic daily data retained")
add_check("WTH012", "Deterministic raw JSON and receipt record a successful HTTP response", file.exists(det_raw) && file.info(det_raw)$size > 0 && nrow(det_receipt) == 1L && is.finite(det_status) && det_status >= 200L && det_status < 300L, paste(if (file.exists(det_raw)) file.info(det_raw)$size else 0, det_status, sep = " / "), "non-empty JSON; HTTP 2xx")
add_check("WTH013", "Deterministic receipt SHA-256 matches the archived JSON", file.exists(det_raw) && nzchar(det_hash_recorded) && identical(tolower(hash_file_sha256(det_raw)), tolower(det_hash_recorded)), paste(hash_file_sha256(det_raw), det_hash_recorded, sep = " / "), "matching SHA-256")

det_coverage_ok <- FALSE
if (deterministic_retained) {
  det_cov <- validate_daily_coverage(weather$deterministic_daily, start_date, end_date)
  write_csv_utf8(det_cov, file.path(weather_dir, "diagnostics", "release_gate_deterministic_coverage.csv"))
  det_coverage_ok <- nrow(det_cov) == 1L && isTRUE(det_cov$complete[[1]])
}
add_check("WTH014", "Deterministic daily data completely cover the audit horizon", det_coverage_ok, if (deterministic_retained) paste(range(as.Date(weather$deterministic_daily$date)), collapse = " to ") else "not retained", paste(expected_dates, collapse = " | "))

response_coordinate_ok <- FALSE
if (nrow(ens_receipt) && all(c("response_latitude", "response_longitude") %in% names(ens_receipt))) {
  rlat <- suppressWarnings(as.numeric(ens_receipt$response_latitude[[1]]))
  rlon <- suppressWarnings(as.numeric(ens_receipt$response_longitude[[1]]))
  response_coordinate_ok <- is.finite(rlat) && is.finite(rlon) && abs(rlat - latitude) <= 0.5 && abs(rlon - longitude) <= 0.5
  response_coordinate_observed <- paste(rlat, rlon, sep = " / ")
} else {
  response_coordinate_observed <- "missing receipt coordinates"
}
add_check("WTH015", "Forecast response coordinates remain close to the requested Azuay point", response_coordinate_ok, response_coordinate_observed, paste(latitude, longitude, sep = " / "))

# -------------------------------------------------------------------------
# SoilGrids live audit
# -------------------------------------------------------------------------
wcs_endpoint <- as.character(app_config$soilgrids_wcs_base_url)
webdav_endpoint <- as.character(app_config$soilgrids_webdav_base_url)
add_check("SOL001", "Configured SoilGrids WCS route is an HTTPS ISRIC endpoint", grepl("^https://", wcs_endpoint) && grepl("isric.org", wcs_endpoint, fixed = TRUE), wcs_endpoint, "HTTPS ISRIC endpoint")
add_check("SOL002", "Configured SoilGrids WebDAV fallback is an HTTPS ISRIC endpoint", grepl("^https://", webdav_endpoint) && grepl("isric.org", webdav_endpoint, fixed = TRUE), webdav_endpoint, "HTTPS ISRIC endpoint")

profiles <- load_crop_profiles_daily()
potato_profile <- get_crop_profile(profiles, "papa_superchola")
soil_warnings <- character(0)
soil_outcome <- withCallingHandlers(
  tryCatch(
    list(
      ok = TRUE,
      value = prepare_soil_input(
        mode = "live_soilgrids",
        profile_row = potato_profile,
        latitude = latitude,
        longitude = longitude,
        app_config = app_config,
        run_dir = soil_dir
      ),
      error = ""
    ),
    error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e))
  ),
  warning = function(w) {
    soil_warnings <<- c(soil_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
writeLines(
  if (length(soil_warnings)) unique(soil_warnings) else "No warnings captured.",
  file.path(soil_dir, "LIVE_SOIL_WARNINGS.txt"),
  useBytes = TRUE
)
if (!soil_outcome$ok) {
  writeLines(soil_outcome$error, file.path(soil_dir, "LIVE_SOIL_ERROR.txt"), useBytes = TRUE)
}
add_check("SOL003", "Live SoilGrids WCS/WebDAV workflow completes", soil_outcome$ok, if (soil_outcome$ok) "completed" else soil_outcome$error, "completed")

soil <- soil_outcome$value
register <- as.data.frame(read_csv_utf8(file.path("config", "soilgrids_layer_register.csv")), stringsAsFactors = FALSE)
required_register_count <- sum(vapply(register$required, parse_bool, logical(1)))
values <- if (!is.null(soil) && is.data.frame(soil$raw_values)) soil$raw_values else data.frame()
layers <- if (!is.null(soil) && is.data.frame(soil$layers)) soil$layers else data.frame()
required_values <- if (nrow(values) && "required" %in% names(values)) values[!is.na(values$required) & values$required, , drop = FALSE] else data.frame()

add_check("SOL004", "SoilGrids register contains the expected 36 required retention layers", required_register_count == 36L, required_register_count, 36L)
required_finite <- nrow(required_values) == 36L && all(is.finite(required_values$converted_value))
add_check("SOL005", "All required field-capacity and wilting-point layers are extracted", required_finite, if (nrow(required_values)) sum(is.finite(required_values$converted_value)) else 0L, "36 / 36")
valid_access_methods <- nrow(required_values) == 36L && all(required_values$access_method %in% c("WCS_GETCOVERAGE", "WEBDAV_VRT"))
add_check("SOL006", "Each required layer records WCS or WebDAV as its acquisition method", valid_access_methods, if (nrow(required_values)) paste(unique(required_values$access_method), collapse = " | ") else "no required values", "WCS_GETCOVERAGE or WEBDAV_VRT")
add_check("SOL007", "SoilGrids access attempts are archived", file.exists(file.path(soil_dir, "soilgrids_access_attempts.csv")) && file.info(file.path(soil_dir, "soilgrids_access_attempts.csv"))$size > 0, if (file.exists(file.path(soil_dir, "soilgrids_access_attempts.csv"))) file.info(file.path(soil_dir, "soilgrids_access_attempts.csv"))$size else 0, ">0 bytes")
add_check("SOL008", "SoilGrids access summary is archived", file.exists(file.path(soil_dir, "soilgrids_access_summary.csv")) && file.info(file.path(soil_dir, "soilgrids_access_summary.csv"))$size > 0, if (file.exists(file.path(soil_dir, "soilgrids_access_summary.csv"))) file.info(file.path(soil_dir, "soilgrids_access_summary.csv"))$size else 0, ">0 bytes")

scenario_names <- if (nrow(layers)) sort(unique(as.character(layers$soil_scenario))) else character(0)
add_check("SOL009", "Live soil integration retains low, central, and high storage profiles", identical(scenario_names, sort(c("low_storage", "central", "high_storage"))), paste(scenario_names, collapse = " | "), "low_storage | central | high_storage")
central_valid <- nrow(layers) > 0L && any(layers$soil_scenario == "central") && all(layers$profile_valid[layers$soil_scenario == "central"])
add_check("SOL010", "Central SoilGrids profile is complete and valid", central_valid, if (nrow(layers)) sum(layers$soil_scenario == "central" & layers$profile_valid) else 0L, ">0 valid central layers")
fc_gt_wp <- nrow(layers) > 0L && all(is.finite(layers$theta_fc) & is.finite(layers$theta_wp) & layers$theta_fc > layers$theta_wp)
add_check("SOL011", "All retained layers satisfy field capacity greater than wilting point", fc_gt_wp, if (nrow(layers)) min(layers$theta_fc - layers$theta_wp) else NA_real_, ">0")
converted_in_range <- nrow(required_values) == 36L && all(required_values$converted_value > 0 & required_values$converted_value < 1)
add_check("SOL012", "Required volumetric retention values lie within the physical fraction range", converted_in_range, if (nrow(required_values)) paste(range(required_values$converted_value, na.rm = TRUE), collapse = " to ") else "no values", "0 < value < 1")

root_summary <- data.frame()
if (nrow(layers)) {
  root_summary <- tryCatch(
    summarise_root_zone_profiles(layers, root_depth_m = as.numeric(potato_profile$zr_scheduling_m[[1]]), p_table = as.numeric(potato_profile$p_table[[1]])),
    error = function(e) data.frame()
  )
  if (nrow(root_summary)) write_csv_utf8(root_summary, file.path(soil_dir, "release_gate_root_zone_summary.csv"))
}
root_taw_ok <- nrow(root_summary) == 3L && all(is.finite(root_summary$taw_mm) & root_summary$taw_mm > 0)
add_check("SOL013", "All three live soil profiles produce positive root-zone available water", root_taw_ok, if (nrow(root_summary)) paste(root_summary$soil_scenario, round(root_summary$taw_mm, 4), collapse = " | ") else "no root-zone summary", "three positive TAW values")

# -------------------------------------------------------------------------
# Archive and package the gate evidence before returning PASS/FAIL.
# -------------------------------------------------------------------------
add_check("ARC001", "R session information is archived", file.exists(file.path(audit_dir, "sessionInfo.txt")) && file.info(file.path(audit_dir, "sessionInfo.txt"))$size > 0, if (file.exists(file.path(audit_dir, "sessionInfo.txt"))) file.info(file.path(audit_dir, "sessionInfo.txt"))$size else 0, ">0 bytes")
source_outputs_present <- dir.exists(weather_dir) && dir.exists(soil_dir) && length(list.files(audit_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)) >= 4L
add_check("ARC002", "Audit directory contains source diagnostics and is ready for hashing", source_outputs_present, length(list.files(audit_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)), ">=4 files")
warning_logs_present <- file.exists(file.path(weather_dir, "LIVE_WEATHER_WARNINGS.txt")) &&
  file.exists(file.path(soil_dir, "LIVE_SOIL_WARNINGS.txt"))
add_check("ARC003", "Weather and soil warning streams are archived without flooding the author console", warning_logs_present, paste(length(weather_warnings), length(soil_warnings), sep = " / "), "two warning logs archived")

all_checks <- dplyr::bind_rows(checks)
write_csv_utf8(all_checks, file.path(audit_dir, "live_source_audit_tests.csv"))
status_text <- if (all(all_checks$status == "PASS")) "PASS" else "FAIL"
status_lines <- c(
  paste0("RiegoAzuay DSS live public-source audit: ", status_text),
  paste0("Run ID: ", run_id),
  paste0("Version: ", version),
  paste0("Audit point: ", latitude, ", ", longitude),
  paste0("Decision horizon: ", start_date, " to ", end_date),
  paste0("Checks passed: ", sum(all_checks$status == "PASS"), " / ", nrow(all_checks)),
  "Boundary: gateway success and payload validity do not establish parcel-scale environmental accuracy."
)
writeLines(status_lines, file.path(audit_dir, "AUDIT_STATUS.txt"), useBytes = TRUE)

manifest <- write_sha256_manifest(audit_dir)
if (!nrow(manifest) || anyNA(manifest$sha256) || any(!nzchar(manifest$sha256))) {
  stop("Live-source audit manifest could not be completed.", call. = FALSE)
}
manifest_checks <- verify_sha256_manifest(audit_dir, stop_on_failure = TRUE)
writeLines(
  c(
    "RiegoAzuay DSS live-source audit manifest verification: PASS",
    paste0("Path-resolved entries: ", nrow(manifest)),
    paste0("Checks passed: ", sum(manifest_checks$status == "PASS"), " / ", nrow(manifest_checks))
  ),
  file.path(audit_dir, "MANIFEST_VERIFICATION.txt"),
  useBytes = TRUE
)
manifest <- write_sha256_manifest(audit_dir)
verify_sha256_manifest(audit_dir, stop_on_failure = TRUE)

bundle_path <- file.path(audit_parent, paste0(run_id, ".zip"))
bundle_abs <- write_verified_zip_app(audit_dir, bundle_path)
bundle_hash <- hash_file_sha256(bundle_abs)
writeLines(
  c(
    paste0(bundle_hash, "  ", basename(bundle_abs)),
    paste0("Audit status: ", status_text),
    paste0("Created UTC: ", format(Sys.time(), tz = "UTC", usetz = TRUE))
  ),
  file.path(audit_parent, paste0(run_id, "_SHA256.txt")),
  useBytes = TRUE
)

cat("\nLive-source audit bundle created:\n", normalizePath(bundle_abs, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat("Bundle SHA-256: ", bundle_hash, "\n", sep = "")
print_test_summary(all_checks, "RESULTADO GENERAL: LA AUDITORÍA DE FUENTES EN VIVO REGISTRÓ FALLOS. Revise y devuelva el bundle generado.")
cat("RESULTADO GENERAL: AUDITORÍA DE FUENTES EN VIVO SUPERADA.\n")
