load_app_config <- function(path = file.path("config", "app_config.csv")) {
  read_key_value_config(path)
}

registry_text_or_blank <- function(x) {
  out <- as.character(x)
  out[is.na(out)] <- ""
  trimws(out)
}

registry_label_key <- function(x) {
  out <- registry_text_or_blank(x)
  ascii <- suppressWarnings(iconv(out, from = "UTF-8", to = "ASCII//TRANSLIT"))
  fallback <- chartr("ÁÉÍÓÚÜÑáéíóúüñ", "AEIOUUNaeiouun", out)
  ascii[is.na(ascii)] <- fallback[is.na(ascii)]
  ascii <- tolower(ascii)
  ascii <- gsub("[^a-z0-9]+", " ", ascii, perl = TRUE)
  trimws(gsub("\\s+", " ", ascii, perl = TRUE))
}

load_administrative_label_crosswalk <- function(
    path = file.path("config", "administrative_label_crosswalk_v1.csv")) {
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  required <- c("level", "source_value", "display_value", "rule_type", "note")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Administrative label crosswalk missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x$level <- tolower(registry_text_or_blank(x$level))
  x$source_value <- registry_text_or_blank(x$source_value)
  x$display_value <- registry_text_or_blank(x$display_value)
  if (any(!x$level %in% c("canton", "parish"))) {
    stop("Administrative label crosswalk has an invalid level.", call. = FALSE)
  }
  if (any(!nzchar(x$source_value)) || any(!nzchar(x$display_value))) {
    stop("Administrative label crosswalk has blank source or display labels.", call. = FALSE)
  }
  keys <- paste(x$level, x$source_value, sep = "\r")
  if (anyDuplicated(keys)) {
    stop("Administrative label crosswalk has duplicated level/source pairs.", call. = FALSE)
  }
  x
}

standardise_administrative_label <- function(x, level, crosswalk = NULL) {
  level <- tolower(as.character(level[[1]]))
  if (!level %in% c("canton", "parish")) stop("Unknown administrative level: ", level, call. = FALSE)
  source <- registry_text_or_blank(x)
  out <- source
  nonblank <- nzchar(source)
  if (!any(nonblank)) return(out)
  if (is.null(crosswalk)) crosswalk <- load_administrative_label_crosswalk()
  lookup_keys <- paste(level, source[nonblank], sep = "\r")
  map_keys <- paste(crosswalk$level, crosswalk$source_value, sep = "\r")
  idx <- match(lookup_keys, map_keys)
  if (anyNA(idx)) {
    missing_values <- unique(source[nonblank][is.na(idx)])
    stop(
      "Administrative label crosswalk does not cover ", level, ": ",
      paste(missing_values, collapse = " | "), call. = FALSE
    )
  }
  out[nonblank] <- crosswalk$display_value[idx]
  out
}

normalise_registry_filter <- function(x, all_token = "__ALL__") {
  if (is.null(x) || length(x) == 0L) return(all_token)
  value <- as.character(x[[1]])
  if (is.na(value) || !nzchar(trimws(value))) return(all_token)
  trimws(value)
}

registry_filter_choices <- function(x) {
  values <- registry_text_or_blank(x)
  values <- values[nzchar(values)]
  if (!length(values)) return(character(0))
  keys <- registry_label_key(values)
  values <- values[!duplicated(keys)]
  sort(values, method = "radix")
}

normalise_system_search_text <- function(x) {
  x <- registry_text_or_blank(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

search_system_registry <- function(registry, query, max_results = 25L) {
  max_results <- max(1L, as.integer(max_results))
  query_key <- normalise_system_search_text(query)
  if (!length(query_key) || !nzchar(query_key[[1]])) {
    return(registry[0, , drop = FALSE])
  }
  query_key <- query_key[[1]]
  tokens <- strsplit(query_key, " ", fixed = TRUE)[[1]]
  tokens <- tokens[nzchar(tokens)]
  system_key <- normalise_system_search_text(registry$system_name)
  id_key <- normalise_system_search_text(registry$public_id)
  # Direct search is intentionally restricted to system name and public application ID.
  # Canton and parish have their own filters; including them here produced irrelevant matches.
  haystack <- trimws(paste(id_key, system_key))
  keep <- rep(TRUE, nrow(registry))
  for (token in tokens) keep <- keep & grepl(token, haystack, fixed = TRUE)
  idx <- which(keep)
  if (!length(idx)) return(registry[0, , drop = FALSE])
  score <- rep(4L, length(idx))
  score[id_key[idx] == query_key] <- 0L
  score[system_key[idx] == query_key] <- pmin(score[system_key[idx] == query_key], 1L)
  score[startsWith(system_key[idx], query_key)] <- pmin(score[startsWith(system_key[idx], query_key)], 2L)
  score[startsWith(id_key[idx], query_key)] <- pmin(score[startsWith(id_key[idx], query_key)], 2L)
  idx <- idx[order(score, system_key[idx], id_key[idx], method = "radix")]
  registry[head(idx, max_results), , drop = FALSE]
}

system_search_exact_match <- function(registry, query) {
  query_key <- normalise_system_search_text(query)
  if (!length(query_key) || !nzchar(query_key[[1]])) return(character(0))
  query_key <- query_key[[1]]
  id_key <- normalise_system_search_text(registry$public_id)
  name_key <- normalise_system_search_text(registry$system_name)
  ids <- registry$public_id[id_key == query_key | name_key == query_key]
  unique(registry_text_or_blank(ids))
}

registry_map_bounds <- function(registry, minimum_span = 0.06, padding_fraction = 0.18) {
  x <- registry_map_rows(registry)
  if (!nrow(x)) return(c(lng1 = -80.3, lat1 = -3.8, lng2 = -78.3, lat2 = -2.4))
  lon_min <- min(x$longitude); lon_max <- max(x$longitude)
  lat_min <- min(x$latitude); lat_max <- max(x$latitude)
  lon_span <- max(lon_max - lon_min, minimum_span)
  lat_span <- max(lat_max - lat_min, minimum_span)
  lon_mid <- (lon_min + lon_max) / 2
  lat_mid <- (lat_min + lat_max) / 2
  lon_pad <- lon_span * padding_fraction
  lat_pad <- lat_span * padding_fraction
  c(
    lng1 = lon_mid - lon_span / 2 - lon_pad,
    lat1 = lat_mid - lat_span / 2 - lat_pad,
    lng2 = lon_mid + lon_span / 2 + lon_pad,
    lat2 = lat_mid + lat_span / 2 + lat_pad
  )
}

haversine_distance_km <- function(lat1, lon1, lat2, lon2) {
  vals <- suppressWarnings(as.numeric(c(lat1, lon1, lat2, lon2)))
  if (length(vals) != 4L || any(!is.finite(vals))) return(NA_real_)
  radius_km <- 6371.0088
  phi1 <- vals[[1]] * pi / 180
  phi2 <- vals[[3]] * pi / 180
  dphi <- (vals[[3]] - vals[[1]]) * pi / 180
  dlambda <- (vals[[4]] - vals[[2]]) * pi / 180
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlambda / 2)^2
  2 * radius_km * asin(min(1, sqrt(a)))
}


polygon_feature_ring_df <- function(feature) {
  if (is.null(feature) || is.null(feature$geometry)) return(NULL)
  geometry_type <- as.character(feature$geometry$type %||% "")
  if (!identical(geometry_type, "Polygon")) return(NULL)
  coords <- feature$geometry$coordinates
  if (!length(coords) || !length(coords[[1]])) return(NULL)
  ring <- coords[[1]]
  mat <- tryCatch(
    if (is.matrix(ring)) ring else do.call(rbind, lapply(ring, unlist)),
    error = function(e) NULL
  )
  if (is.null(mat) || ncol(mat) < 2L) return(NULL)
  out <- data.frame(lng = suppressWarnings(as.numeric(mat[, 1])),
                    lat = suppressWarnings(as.numeric(mat[, 2])))
  out <- out[is.finite(out$lng) & is.finite(out$lat), , drop = FALSE]
  if (nrow(out) < 3L) return(NULL)
  if (!isTRUE(all.equal(out[1, c("lng", "lat")], out[nrow(out), c("lng", "lat")], tolerance = 1e-12))) {
    out <- rbind(out, out[1, , drop = FALSE])
  }
  unique_vertices <- unique(out[-nrow(out), c("lng", "lat"), drop = FALSE])
  if (nrow(unique_vertices) < 3L) return(NULL)
  rownames(out) <- NULL
  out
}

polygon_area_centroid_wgs84 <- function(ring_df) {
  ring <- as.data.frame(ring_df, stringsAsFactors = FALSE)
  if (!all(c("lng", "lat") %in% names(ring)) || nrow(ring) < 4L) {
    stop("Polygon ring must contain at least three vertices plus closure.", call. = FALSE)
  }
  lng <- suppressWarnings(as.numeric(ring$lng))
  lat <- suppressWarnings(as.numeric(ring$lat))
  if (any(!is.finite(lng)) || any(!is.finite(lat))) {
    stop("Polygon ring contains non-finite coordinates.", call. = FALSE)
  }
  if (!isTRUE(all.equal(c(lng[[1]], lat[[1]]), c(lng[[length(lng)]], lat[[length(lat)]]), tolerance = 1e-12))) {
    lng <- c(lng, lng[[1]])
    lat <- c(lat, lat[[1]])
  }
  radius_m <- 6371008.8
  lat0 <- mean(lat[-length(lat)]) * pi / 180
  cos_lat0 <- cos(lat0)
  if (!is.finite(cos_lat0) || abs(cos_lat0) < 1e-8) {
    stop("Polygon latitude is unsuitable for the local projection.", call. = FALSE)
  }
  x_abs <- radius_m * (lng * pi / 180) * cos_lat0
  y_abs <- radius_m * (lat * pi / 180)
  # Translate coordinates near the origin before the shoelace calculation to
  # reduce floating-point cancellation for small parcels at large map coordinates.
  x_origin <- mean(x_abs[-length(x_abs)])
  y_origin <- mean(y_abs[-length(y_abs)])
  x <- x_abs - x_origin
  y <- y_abs - y_origin
  cross <- x[-length(x)] * y[-1] - x[-1] * y[-length(y)]
  twice_signed_area <- sum(cross)
  area_m2 <- abs(twice_signed_area) / 2
  if (!is.finite(area_m2) || area_m2 <= 0) {
    stop("Polygon area is zero or invalid.", call. = FALSE)
  }
  if (abs(twice_signed_area) > .Machine$double.eps) {
    centroid_x_local <- sum((x[-length(x)] + x[-1]) * cross) / (3 * twice_signed_area)
    centroid_y_local <- sum((y[-length(y)] + y[-1]) * cross) / (3 * twice_signed_area)
  } else {
    centroid_x_local <- mean(x[-length(x)])
    centroid_y_local <- mean(y[-length(y)])
  }
  centroid_x <- centroid_x_local + x_origin
  centroid_y <- centroid_y_local + y_origin
  centroid_lng <- centroid_x / (radius_m * cos_lat0) * 180 / pi
  centroid_lat <- centroid_y / radius_m * 180 / pi
  list(
    area_m2 = area_m2,
    area_ha = area_m2 / 10000,
    centroid_latitude = centroid_lat,
    centroid_longitude = centroid_lng,
    vertex_count = length(lng) - 1L,
    method = "local_equirectangular_shoelace"
  )
}

polygon_metrics_from_feature <- function(feature) {
  ring <- polygon_feature_ring_df(feature)
  if (is.null(ring)) return(NULL)
  metrics <- polygon_area_centroid_wgs84(ring)
  metrics$ring <- ring
  metrics
}

registry_name_ambiguity_audit <- function(registry) {
  name_key <- normalise_system_search_text(registry$system_name)
  duplicated_name <- duplicated(name_key) | duplicated(name_key, fromLast = TRUE)
  combined_parish <- grepl("\\by\\b|/|;", normalise_system_search_text(registry$parish_source))
  out <- registry[duplicated_name | combined_parish, c(
    "public_id", "system_name", "canton_source", "parish_source",
    "canton_display", "parish_display", "latitude", "longitude"
  ), drop = FALSE]
  out$ambiguity_type <- ifelse(
    duplicated_name[duplicated_name | combined_parish] & combined_parish[duplicated_name | combined_parish],
    "duplicate_name_and_combined_parish_label",
    ifelse(duplicated_name[duplicated_name | combined_parish], "duplicate_system_name", "combined_parish_source_label")
  )
  out
}

filter_system_registry <- function(registry, canton = "__ALL__", parish = "__ALL__") {
  canton <- normalise_registry_filter(canton)
  parish <- normalise_registry_filter(parish)
  canton_column <- if ("canton_display" %in% names(registry)) registry$canton_display else registry$canton
  parish_column <- if ("parish_display" %in% names(registry)) registry$parish_display else registry$parish
  canton_values <- registry_text_or_blank(canton_column)
  parish_values <- registry_text_or_blank(parish_column)
  keep <- rep(TRUE, nrow(registry))
  if (!identical(canton, "__ALL__")) keep <- keep & canton_values == canton
  if (!identical(parish, "__ALL__")) keep <- keep & parish_values == parish
  registry[which(keep), , drop = FALSE]
}

registry_map_rows <- function(registry) {
  latitude <- suppressWarnings(as.numeric(registry$latitude))
  longitude <- suppressWarnings(as.numeric(registry$longitude))
  public_id <- registry_text_or_blank(registry$public_id)
  keep <- is.finite(latitude) & is.finite(longitude) &
    latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180 &
    nzchar(public_id)
  out <- registry[which(keep), , drop = FALSE]
  out$latitude <- latitude[keep]
  out$longitude <- longitude[keep]
  out
}

resolve_registry_selection <- function(value, registry, fallback_id = NULL) {
  ids <- registry_text_or_blank(registry$public_id)
  if (!length(ids)) stop("No selectable irrigation-system records are available.", call. = FALSE)
  candidate <- if (is.null(value) || length(value) == 0L) "" else registry_text_or_blank(value[[1]])
  if (nzchar(candidate) && candidate %in% ids) return(candidate)
  fallback <- if (is.null(fallback_id) || length(fallback_id) == 0L) "" else registry_text_or_blank(fallback_id[[1]])
  if (nzchar(fallback) && fallback %in% ids) return(fallback)
  ids[[1]]
}

load_system_registry <- function(config) {
  restricted_path <- as.character(config$restricted_registry_path)
  demo_path <- as.character(config$public_demo_registry_path)
  path <- if (file.exists(restricted_path)) restricted_path else demo_path
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  required <- c("public_id", "system_name", "canton", "parish", "latitude", "longitude",
                "coordinate_semantics", "source_status")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("System registry missing: ", paste(missing, collapse = ", "), call. = FALSE)
  x$public_id <- registry_text_or_blank(x$public_id)
  x$system_name <- registry_text_or_blank(x$system_name)
  x$latitude <- suppressWarnings(as.numeric(x$latitude))
  x$longitude <- suppressWarnings(as.numeric(x$longitude))
  if (!"altitude_m" %in% names(x)) x$altitude_m <- NA_real_
  x$altitude_m <- suppressWarnings(as.numeric(x$altitude_m))
  if (any(!nzchar(x$public_id))) stop("System registry has blank public IDs.", call. = FALSE)
  if (any(!nzchar(x$system_name))) stop("System registry has blank system names.", call. = FALSE)
  if (anyDuplicated(x$public_id)) stop("System registry has duplicated public IDs.", call. = FALSE)
  if (any(!is.finite(x$latitude)) || any(!is.finite(x$longitude))) {
    stop("System registry has invalid coordinates.", call. = FALSE)
  }
  crosswalk <- load_administrative_label_crosswalk()
  x$canton_source <- registry_text_or_blank(x$canton)
  x$parish_source <- registry_text_or_blank(x$parish)
  x$canton_display <- standardise_administrative_label(x$canton_source, "canton", crosswalk)
  x$parish_display <- standardise_administrative_label(x$parish_source, "parish", crosswalk)
  x$display_label <- paste0(
    x$public_id, " — ", x$system_name,
    ifelse(nzchar(x$canton_display), paste0(" | ", x$canton_display), ""),
    ifelse(nzchar(x$parish_display), paste0(" / ", x$parish_display), "")
  )
  if (any(is.na(x$display_label) | !nzchar(x$display_label))) {
    stop("System registry produced invalid selector labels.", call. = FALSE)
  }
  attr(x, "registry_mode") <- if (identical(path, restricted_path)) "restricted" else "demo"
  attr(x, "registry_path") <- path
  x[order(x$public_id), , drop = FALSE]
}

load_evidence_levels <- function(path = file.path("config", "evidence_levels_v1.csv")) {
  as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
}

load_sdg_layer <- function(path = file.path("config", "sdg_information_layer.csv")) {
  as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
}

load_provenance_classes <- function(path = file.path("config", "provenance_classes.csv")) {
  as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
}

load_initial_state_presets <- function(path = file.path("config", "simple_initial_state_presets_v1.csv")) {
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  required <- c("preset_id", "label_es", "label_en", "initial_mode", "initial_value", "verified", "provenance_class")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Initial-state preset table missing: ", paste(missing, collapse = ", "), call. = FALSE)
  x$preset_id <- trimws(as.character(x$preset_id))
  x$initial_mode <- trimws(as.character(x$initial_mode))
  x$initial_value <- as.numeric(x$initial_value)
  x$verified <- as.logical(x$verified)
  if (any(!nzchar(x$preset_id)) || anyDuplicated(x$preset_id)) stop("Initial-state preset IDs must be complete and unique.", call. = FALSE)
  if (any(!x$initial_mode %in% c("field_capacity", "fraction_raw", "fraction_taw", "mm", "measured_theta"))) stop("Initial-state preset table has an invalid mode.", call. = FALSE)
  if (any(!is.finite(x$initial_value))) stop("Initial-state preset values must be finite.", call. = FALSE)
  x
}

registry_choice_vector <- function(registry) {
  ids <- registry_text_or_blank(registry$public_id)
  labels <- registry_text_or_blank(registry$display_label)
  if (any(!nzchar(ids)) || any(!nzchar(labels)) || anyDuplicated(ids)) {
    stop("System registry contains invalid selector values or labels.", call. = FALSE)
  }
  stats::setNames(ids, labels)
}

selected_registry_row <- function(registry, public_id) {
  public_id <- registry_text_or_blank(public_id[[1]])
  ids <- registry_text_or_blank(registry$public_id)
  z <- registry[which(ids == public_id), , drop = FALSE]
  if (nrow(z) != 1L) stop("Unknown or duplicated system public ID: ", public_id, call. = FALSE)
  z
}

standard_depth_boundaries <- function(max_depth_m = 2) {
  base <- c(0, 0.05, 0.15, 0.30, 0.60, 1.00, 2.00)
  max_depth_m <- as.numeric(max_depth_m)
  if (!is.finite(max_depth_m) || max_depth_m <= 0) stop("max_depth_m must be positive.", call. = FALSE)
  out <- base[base < max_depth_m]
  unique(c(out, max_depth_m))
}

make_uniform_soil_layers <- function(theta_fc, theta_wp, max_depth_m = 2,
                                     soil_scenario = "reference",
                                     source_type = "PUBLISHED_REFERENCE",
                                     source_note = "") {
  theta_fc <- as.numeric(theta_fc); theta_wp <- as.numeric(theta_wp)
  if (!is.finite(theta_fc) || !is.finite(theta_wp) || theta_fc <= theta_wp) {
    stop("Uniform soil requires finite theta_fc > theta_wp.", call. = FALSE)
  }
  b <- standard_depth_boundaries(max_depth_m)
  data.frame(
    soil_scenario = soil_scenario,
    depth_interval = paste0(round(b[-length(b)] * 100), "-", round(b[-1] * 100), "cm"),
    depth_top_m = b[-length(b)],
    depth_bottom_m = b[-1],
    theta_fc = theta_fc,
    theta_wp = theta_wp,
    available_water_fraction = theta_fc - theta_wp,
    field_capacity_quantile = NA_character_,
    wilting_point_quantile = NA_character_,
    sand_fraction = NA_real_,
    silt_fraction = NA_real_,
    clay_fraction = NA_real_,
    profile_valid = TRUE,
    source_type = source_type,
    uncertainty_interpretation = source_note,
    stringsAsFactors = FALSE
  )
}

load_archived_santul_soil_layers <- function(path = file.path("data", "demo", "soil_layer_profiles_santul_archived.csv")) {
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  x[c("depth_top_m", "depth_bottom_m", "theta_fc", "theta_wp")] <-
    lapply(x[c("depth_top_m", "depth_bottom_m", "theta_fc", "theta_wp")], as.numeric)
  x
}

build_relative_weather_fixture <- function(start_date, end_date,
                                           path = file.path("data", "demo", "weather_relative_fixture.csv")) {
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || end_date < start_date) stop("Invalid weather horizon.", call. = FALSE)
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  x$day_offset <- as.integer(x$day_offset)
  horizon <- as.integer(end_date - start_date)
  if (horizon > max(x$day_offset)) {
    stop("Offline demonstration weather data support at most ", max(x$day_offset) + 1L, " daily values.", call. = FALSE)
  }
  x <- x[x$day_offset <= horizon, , drop = FALSE]
  x$date <- start_date + x$day_offset
  x[, c("weather_scenario", "date", "eto_mm_day", "precip_mm_day", "forecast_source")]
}

build_shifted_archived_weather <- function(start_date, end_date,
                                           path = file.path("data", "demo", "archived_selected_forecast_20261025.csv")) {
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  x$date <- as.Date(x$date)
  min_date <- min(x$date)
  x$day_offset <- as.integer(x$date - min_date)
  horizon <- as.integer(end_date - start_date)
  if (horizon > max(x$day_offset)) {
    stop("The saved demonstration forecast does not cover the requested horizon.", call. = FALSE)
  }
  x <- x[x$day_offset <= horizon, , drop = FALSE]
  x$date <- start_date + x$day_offset
  x$forecast_source <- "ARCHIVED_ECMWF_FIXTURE_SHIFTED_FOR_SOFTWARE_TESTING"
  x[, c("weather_scenario", "date", "eto_mm_day", "precip_mm_day",
        "selected_member_id", "forecast_source")]
}

read_manual_weather <- function(path, start_date, end_date) {
  if (is.null(path) || !file.exists(path)) stop("A weather CSV file is required.", call. = FALSE)
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  required <- c("date", "eto_mm_day", "precip_mm_day")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Weather CSV missing: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!"weather_scenario" %in% names(x)) x$weather_scenario <- "central"
  if (!"source_note" %in% names(x)) x$source_note <- "USER_SUPPLIED_WEATHER"
  x$date <- as.Date(x$date)
  x$eto_mm_day <- as.numeric(x$eto_mm_day)
  x$precip_mm_day <- as.numeric(x$precip_mm_day)
  if (anyNA(x$date) || any(!is.finite(x$eto_mm_day)) || any(!is.finite(x$precip_mm_day)) ||
      any(x$eto_mm_day < 0) || any(x$precip_mm_day < 0)) {
    stop("Weather CSV contains invalid dates or values.", call. = FALSE)
  }
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)
  expected <- seq(start_date, end_date, by = "day")
  for (s in unique(x$weather_scenario)) {
    got <- sort(unique(x$date[x$weather_scenario == s]))
    missing_dates <- setdiff(expected, got)
    if (length(missing_dates)) {
      stop("Weather scenario '", s, "' is missing: ", paste(missing_dates, collapse = ", "), call. = FALSE)
    }
  }
  x <- x[x$date >= start_date & x$date <= end_date, , drop = FALSE]
  x$forecast_source <- "USER_SUPPLIED_OR_LOCAL_STATION_CSV"
  x
}

crop_age_on_date <- function(profile_row, mode, planting_date, declared_age_days, reference_date) {
  reference_date <- as.Date(reference_date)
  if (identical(mode, "planting_date")) {
    planting_date <- as.Date(planting_date)
    age <- as.integer(reference_date - planting_date) + 1L
  } else {
    age <- as.integer(declared_age_days)
  }
  if (!is.finite(age) || age < 1L || age > as.integer(profile_row$cycle_days[[1]])) {
    stop("Crop age must be between 1 and ", profile_row$cycle_days[[1]], " days.", call. = FALSE)
  }
  age
}

initial_depletion_layered <- function(mode, value, soil_layers, root_depth_m,
                                      p_table, etc_potential_mm_day = 5) {
  layers <- validate_layered_soil(soil_layers)
  taw <- layered_taw_mm(layers, root_depth_m)
  mode <- as.character(mode)
  value <- suppressWarnings(as.numeric(value))
  if (mode == "field_capacity") return(0)
  if (mode == "mm") return(clamp_value(value, 0, taw))
  if (mode == "fraction_taw") return(clamp_value(value, 0, 1) * taw)
  if (mode == "fraction_raw") {
    p_adj <- adjust_depletion_fraction(p_table, etc_potential_mm_day, enabled = TRUE)
    return(clamp_value(value, 0, 2) * calc_raw_mm(taw, p_adj))
  }
  if (mode == "measured_theta") {
    theta <- value
    if (!is.finite(theta) || theta < 0 || theta > 1) stop("Measured theta must be in [0,1].", call. = FALSE)
    overlap <- layer_overlap_m(layers, root_depth_m)
    depletion <- sum(1000 * pmax(layers$theta_fc - theta, 0) * overlap)
    return(clamp_value(depletion, 0, taw))
  }
  stop("Unknown initial water mode: ", mode, call. = FALSE)
}

normalise_weather_scenario_labels <- function(x) {
  z <- as.character(x)
  z[z %in% c("wet", "low", "p10")] <- "lower"
  z[z %in% c("dry", "high", "p90")] <- "upper"
  z
}

hash_file_sha256 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(path, algo = "sha256", file = TRUE)
}

safe_character <- function(x, default = "") {
  if (is.null(x) || !length(x) || all(is.na(x))) default else as.character(x[[1]])
}
