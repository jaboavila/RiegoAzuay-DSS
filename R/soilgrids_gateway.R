parse_depth_interval <- function(x) {
  z <- gsub("cm", "", trimws(as.character(x)), fixed = TRUE)
  parts <- strsplit(z, "-", fixed = TRUE)[[1]]
  if (length(parts) != 2L) stop("Invalid depth interval: ", x, call. = FALSE)
  top_cm <- suppressWarnings(as.numeric(parts[[1]]))
  bottom_cm <- suppressWarnings(as.numeric(parts[[2]]))
  if (!is.finite(top_cm) || !is.finite(bottom_cm) || bottom_cm <= top_cm) stop("Invalid depth interval: ", x, call. = FALSE)
  data.frame(depth_top_cm = top_cm, depth_bottom_cm = bottom_cm,
             depth_top_m = top_cm / 100, depth_bottom_m = bottom_cm / 100)
}

soilgrids_statistic_candidates <- function(statistic, service = c("wcs", "webdav")) {
  service <- match.arg(service)
  statistic <- as.character(statistic)
  if (service == "wcs") {
    switch(statistic,
           Q05 = c("Q0.05", "Q05"),
           Q50 = c("Q0.5", "Q50", "Q0.50"),
           Q95 = c("Q0.95", "Q95"),
           mean = "mean",
           stop("Unsupported SoilGrids statistic: ", statistic, call. = FALSE))
  } else {
    switch(statistic,
           Q05 = c("Q05", "Q0.05"),
           Q50 = c("Q50", "Q0.5", "Q0.50"),
           Q95 = c("Q95", "Q0.95"),
           mean = "mean",
           stop("Unsupported SoilGrids statistic: ", statistic, call. = FALSE))
  }
}

soilgrids_property_candidates <- function(property_code, property_alias = "") {
  x <- c(as.character(property_code), trimws(as.character(property_alias)))
  unique(x[nzchar(x)])
}

soilgrids_service_url <- function(wcs_base_url, property_name) {
  paste0(sub("[?&]$", "", wcs_base_url), "?map=/map/", property_name, ".map")
}

soilgrids_wcs_capabilities_url <- function(wcs_base_url, property_name, version = "2.0.1") {
  paste0(soilgrids_service_url(wcs_base_url, property_name),
         "&SERVICE=WCS&VERSION=", utils::URLencode(version, reserved = TRUE),
         "&REQUEST=GetCapabilities")
}

parse_wcs_coverage_ids <- function(xml_path_or_text, is_text = FALSE) {
  doc <- if (is_text) xml2::read_xml(xml_path_or_text) else xml2::read_xml(xml_path_or_text)
  nodes <- xml2::xml_find_all(doc, ".//*[local-name()='CoverageId' or local-name()='Identifier']")
  ids <- trimws(xml2::xml_text(nodes))
  unique(ids[nzchar(ids)])
}

match_soilgrids_coverage_id <- function(coverage_ids, property_candidates,
                                        depth_interval, statistic) {
  stats <- soilgrids_statistic_candidates(statistic, "wcs")
  expected <- as.vector(outer(property_candidates, stats,
                              function(p, s) paste0(p, "_", depth_interval, "_", s)))
  exact <- coverage_ids[coverage_ids %in% expected]
  if (length(exact)) return(exact[[1]])
  norm <- function(x) tolower(gsub("[^a-z0-9]", "", x))
  ids_norm <- norm(coverage_ids)
  expected_norm <- norm(expected)
  idx <- match(expected_norm, ids_norm, nomatch = 0L)
  idx <- idx[idx > 0L]
  if (length(idx)) return(coverage_ids[[idx[[1]]]])
  NA_character_
}

point_to_soilgrids_projection <- function(longitude, latitude, projection) {
  if (!is.finite(longitude) || longitude < -180 || longitude > 180) stop("Invalid longitude.", call. = FALSE)
  if (!is.finite(latitude) || latitude < -90 || latitude > 90) stop("Invalid latitude.", call. = FALSE)
  pt <- terra::vect(data.frame(longitude = longitude, latitude = latitude),
                    geom = c("longitude", "latitude"), crs = "EPSG:4326")
  terra::project(pt, projection)
}

projected_point_xy <- function(point_projected) {
  z <- terra::crds(point_projected, df = TRUE)
  c(x = as.numeric(z[[1]][[1]]), y = as.numeric(z[[2]][[1]]))
}

soilgrids_wcs_bbox <- function(point_projected, half_size_m = 375) {
  xy <- projected_point_xy(point_projected)
  if (!is.finite(half_size_m) || half_size_m <= 0) stop("Invalid WCS half-size.", call. = FALSE)
  c(xmin = xy[["x"]] - half_size_m, xmax = xy[["x"]] + half_size_m,
    ymin = xy[["y"]] - half_size_m, ymax = xy[["y"]] + half_size_m)
}

format_wcs_number <- function(x) formatC(as.numeric(x), digits = 3, format = "f", decimal.mark = ".")

soilgrids_wcs_getcoverage_url <- function(wcs_base_url, service_property,
                                           coverage_id, bbox,
                                           version = "2.0.1",
                                           crs_uri = "http://www.opengis.net/def/crs/EPSG/0/152160",
                                           format_name = "GEOTIFF_INT16") {
  q <- c(
    "SERVICE=WCS",
    paste0("VERSION=", utils::URLencode(version, reserved = TRUE)),
    "REQUEST=GetCoverage",
    paste0("COVERAGEID=", utils::URLencode(coverage_id, reserved = TRUE)),
    paste0("FORMAT=", utils::URLencode(format_name, reserved = TRUE)),
    paste0("SUBSET=X(", format_wcs_number(bbox[["xmin"]]), ",", format_wcs_number(bbox[["xmax"]]), ")"),
    paste0("SUBSET=Y(", format_wcs_number(bbox[["ymin"]]), ",", format_wcs_number(bbox[["ymax"]]), ")"),
    paste0("SUBSETTINGCRS=", utils::URLencode(crs_uri, reserved = TRUE)),
    paste0("OUTPUTCRS=", utils::URLencode(crs_uri, reserved = TRUE))
  )
  paste0(soilgrids_service_url(wcs_base_url, service_property), "&", paste(q, collapse = "&"))
}

safe_filename <- function(x) gsub("[^A-Za-z0-9._-]+", "_", as.character(x))

perform_http_to_file <- function(url, destination, timeout_seconds = 120,
                                 max_tries = 3L) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  req <- httr2::request(url) |>
    httr2::req_user_agent("RiegoAzuay-DSS/2.2.3 SoilGrids research client") |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_retry(max_tries = max_tries)
  resp <- httr2::req_perform(req, path = destination)
  status <- httr2::resp_status(resp)
  if (status < 200L || status >= 300L) stop("HTTP status ", status, call. = FALSE)
  if (!file.exists(destination) || file.info(destination)$size <= 0) stop("Downloaded file is empty.", call. = FALSE)
  invisible(resp)
}

fetch_wcs_capabilities <- function(wcs_base_url, property_candidates, cache_dir,
                                   version = "2.0.1", timeout_seconds = 120) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  attempts <- list(); k <- 1L
  versions <- unique(c(as.character(version), "2.0.0"))
  for (p in property_candidates) {
    for (ver in versions) {
      url <- soilgrids_wcs_capabilities_url(wcs_base_url, p, ver)
      dest <- file.path(cache_dir, paste0("WCS_GetCapabilities_", safe_filename(p), "_", safe_filename(ver), ".xml"))
      result <- try({
        if (!file.exists(dest) || file.info(dest)$size <= 0) perform_http_to_file(url, dest, timeout_seconds)
        ids <- parse_wcs_coverage_ids(dest)
        if (!length(ids)) stop("No coverage identifiers found.")
        list(service_property = p, capabilities_version = ver, url = url, file = dest, coverage_ids = ids)
      }, silent = TRUE)
      if (!inherits(result, "try-error")) {
        attempts[[k]] <- data.frame(access_method = "WCS_CAPABILITIES", service_property = p,
                                    request_url = url, status = "SUCCESS", message = "",
                                    stringsAsFactors = FALSE)
        return(c(result, list(attempts = dplyr::bind_rows(attempts))))
      }
      attempts[[k]] <- data.frame(access_method = "WCS_CAPABILITIES", service_property = p,
                                  request_url = url, status = "FAIL",
                                  message = substr(as.character(result), 1, 800), stringsAsFactors = FALSE)
      k <- k + 1L
    }
  }
  list(service_property = NA_character_, capabilities_version = NA_character_, url = NA_character_,
       file = NA_character_, coverage_ids = character(0), attempts = dplyr::bind_rows(attempts))
}

extract_value_from_local_raster <- function(raster_path, point_projected,
                                            projection_fallback = NULL) {
  r <- terra::rast(raster_path)
  if ((is.na(terra::crs(r)) || !nzchar(terra::crs(r))) && text_present(projection_fallback)) {
    terra::crs(r) <- projection_fallback
  }
  v <- terra::extract(r, point_projected, ID = FALSE)
  value <- suppressWarnings(as.numeric(v[[1]][[1]]))
  if (!is.finite(value)) {
    xy <- projected_point_xy(point_projected)
    cell <- terra::cellFromXY(r, matrix(c(xy[["x"]], xy[["y"]]), ncol = 2))
    value <- suppressWarnings(as.numeric(terra::values(r, mat = FALSE)[cell[[1]]]))
  }
  if (!is.finite(value)) stop("nonfinite extracted value", call. = FALSE)
  value
}

extract_wcs_value <- function(wcs_base_url, property_candidates, depth_interval,
                              statistic, point_projected, cache_dir,
                              version = "2.0.1", half_size_m = 375,
                              crs_uri = "http://www.opengis.net/def/crs/EPSG/0/152160",
                              projection_fallback = NULL,
                              timeout_seconds = 120) {
  cap <- fetch_wcs_capabilities(wcs_base_url, property_candidates, cache_dir,
                                version = version, timeout_seconds = timeout_seconds)
  attempts <- cap$attempts
  if (!length(cap$coverage_ids)) {
    return(list(value = NA_real_, selected_url = NA_character_, selected_coverage = NA_character_,
                service_property = NA_character_, access_method = NA_character_, attempts = attempts))
  }
  coverage_id <- match_soilgrids_coverage_id(cap$coverage_ids, property_candidates,
                                              depth_interval, statistic)
  if (!text_present(coverage_id)) {
    attempts <- dplyr::bind_rows(attempts, data.frame(
      access_method = "WCS_COVERAGE_MATCH", service_property = cap$service_property,
      request_url = cap$url, status = "FAIL",
      message = paste0("Coverage not found for ", paste(property_candidates, collapse = "/"),
                       " ", depth_interval, " ", statistic, ". Available: ",
                       paste(cap$coverage_ids, collapse = " | ")), stringsAsFactors = FALSE))
    return(list(value = NA_real_, selected_url = NA_character_, selected_coverage = NA_character_,
                service_property = cap$service_property, access_method = NA_character_, attempts = attempts))
  }
  bbox <- soilgrids_wcs_bbox(point_projected, half_size_m)
  formats <- c("GEOTIFF_INT16", "image/tiff")
  for (fmt in formats) {
    url <- soilgrids_wcs_getcoverage_url(wcs_base_url, cap$service_property, coverage_id,
                                         bbox, version, crs_uri, fmt)
    dest <- file.path(cache_dir, paste0("WCS_", safe_filename(coverage_id), "_", safe_filename(fmt), ".tif"))
    result <- try({
      if (!file.exists(dest) || file.info(dest)$size <= 0) perform_http_to_file(url, dest, timeout_seconds)
      value <- extract_value_from_local_raster(dest, point_projected, projection_fallback)
      list(value = value, url = url, file = dest)
    }, silent = TRUE)
    if (!inherits(result, "try-error")) {
      attempts <- dplyr::bind_rows(attempts, data.frame(
        access_method = "WCS_GETCOVERAGE", service_property = cap$service_property,
        request_url = url, status = "SUCCESS", message = "",
        stringsAsFactors = FALSE))
      return(list(value = result$value, selected_url = result$url,
                  selected_coverage = coverage_id, service_property = cap$service_property,
                  access_method = "WCS_GETCOVERAGE", local_file = result$file,
                  attempts = attempts))
    }
    attempts <- dplyr::bind_rows(attempts, data.frame(
      access_method = "WCS_GETCOVERAGE", service_property = cap$service_property,
      request_url = url, status = "FAIL", message = substr(as.character(result), 1, 800),
      stringsAsFactors = FALSE))
  }
  list(value = NA_real_, selected_url = NA_character_, selected_coverage = coverage_id,
       service_property = cap$service_property, access_method = NA_character_, attempts = attempts)
}

build_soilgrids_webdav_url_candidates <- function(base_url, property_code, property_alias,
                                                    depth_interval, statistic) {
  props <- soilgrids_property_candidates(property_code, property_alias)
  stats <- soilgrids_statistic_candidates(statistic, "webdav")
  urls <- character(0)
  for (p in props) {
    for (s in stats) {
      filename <- paste0(p, "_", depth_interval, "_", s, ".vrt")
      urls <- c(urls, paste0(sub("/$", "", base_url), "/", p, "/", filename))
    }
  }
  unique(urls)
}

soilgrids_vsicurl_candidates <- function(url) {
  c(paste0("/vsicurl/", url),
    paste0("/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=", url))
}

extract_webdav_vrt_value <- function(url_candidates, point_projected,
                                     projection_fallback = NULL) {
  attempts <- list(); k <- 1L
  terra::setGDALconfig("GDAL_HTTP_MAX_RETRY", "3")
  terra::setGDALconfig("GDAL_HTTP_RETRY_DELAY", "1")
  terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".vrt,.tif,.ovr")
  for (url in url_candidates) {
    for (path in soilgrids_vsicurl_candidates(url)) {
      warnings_seen <- character(0)
      result <- withCallingHandlers(try({
        r <- terra::rast(path)
        v <- terra::extract(r, point_projected, ID = FALSE)
        value <- suppressWarnings(as.numeric(v[[1]][[1]]))
        if (!is.finite(value)) stop("nonfinite extracted value")
        list(value = value, url = url, path = path)
      }, silent = TRUE), warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
      if (!inherits(result, "try-error")) {
        attempts[[k]] <- data.frame(access_method = "WEBDAV_VRT", service_property = "",
                                    request_url = url, status = "SUCCESS",
                                    message = paste(unique(warnings_seen), collapse = " | "),
                                    stringsAsFactors = FALSE)
        return(list(value = result$value, selected_url = result$url,
                    selected_coverage = NA_character_, service_property = NA_character_,
                    access_method = "WEBDAV_VRT", attempts = dplyr::bind_rows(attempts)))
      }
      attempts[[k]] <- data.frame(access_method = "WEBDAV_VRT", service_property = "",
                                  request_url = url, status = "FAIL",
                                  message = substr(paste(c(as.character(result), warnings_seen), collapse = " | "), 1, 800),
                                  stringsAsFactors = FALSE)
      k <- k + 1L
    }
  }
  list(value = NA_real_, selected_url = NA_character_, selected_coverage = NA_character_,
       service_property = NA_character_, access_method = NA_character_,
       attempts = dplyr::bind_rows(attempts))
}

query_soilgrids_register <- function(register, longitude, latitude,
                                     webdav_base_url, wcs_base_url,
                                     projection, output_dir,
                                     access_order = c("wcs", "webdav"),
                                     wcs_version = "2.0.1",
                                     wcs_half_size_m = 375,
                                     wcs_crs_uri = "http://www.opengis.net/def/crs/EPSG/0/152160",
                                     timeout_seconds = 120,
                                     pause_seconds = 0.15) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  cache_dir <- file.path(output_dir, "soilgrids_cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  point_projected <- point_to_soilgrids_projection(longitude, latitude, projection)
  values <- list(); attempts <- list(); k <- 1L
  access_order <- trimws(tolower(as.character(access_order)))
  access_order <- access_order[access_order %in% c("wcs", "webdav")]
  if (!length(access_order)) stop("SoilGrids access order is empty.", call. = FALSE)
  for (i in seq_len(nrow(register))) {
    row <- register[i, , drop = FALSE]
    props <- soilgrids_property_candidates(row$property_code[[1]], row$property_alias[[1]] %||% "")
    z <- NULL
    layer_attempts <- list()
    for (method in access_order) {
      if (method == "wcs") {
        z0 <- extract_wcs_value(
          wcs_base_url = wcs_base_url,
          property_candidates = props,
          depth_interval = row$depth_interval[[1]],
          statistic = row$statistic[[1]],
          point_projected = point_projected,
          cache_dir = file.path(cache_dir, "wcs"),
          version = wcs_version,
          half_size_m = wcs_half_size_m,
          crs_uri = wcs_crs_uri,
          projection_fallback = projection,
          timeout_seconds = timeout_seconds
        )
      } else {
        urls <- build_soilgrids_webdav_url_candidates(
          webdav_base_url, row$property_code[[1]], row$property_alias[[1]] %||% "",
          row$depth_interval[[1]], row$statistic[[1]])
        z0 <- extract_webdav_vrt_value(urls, point_projected, projection)
      }
      if (nrow(z0$attempts)) layer_attempts[[length(layer_attempts)+1L]] <- z0$attempts
      if (is.finite(z0$value)) { z <- z0; break }
    }
    if (is.null(z)) z <- list(value = NA_real_, selected_url = NA_character_, selected_coverage = NA_character_,
                              service_property = NA_character_, access_method = NA_character_)
    conversion <- as.numeric(row$conversion_to_fraction[[1]])
    converted <- if (is.finite(z$value)) z$value * conversion else NA_real_
    values[[i]] <- data.frame(
      property_role = row$property_role[[1]],
      property_code_requested = row$property_code[[1]],
      property_alias = row$property_alias[[1]],
      depth_interval = row$depth_interval[[1]],
      statistic = row$statistic[[1]],
      raw_value = z$value,
      conversion_to_fraction = conversion,
      converted_value = converted,
      selected_url = z$selected_url,
      selected_coverage = z$selected_coverage,
      service_property = z$service_property,
      access_method = z$access_method,
      required = parse_bool(row$required[[1]]),
      retrieval_timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      stringsAsFactors = FALSE
    )
    a <- dplyr::bind_rows(layer_attempts)
    if (nrow(a)) {
      a$register_row <- i
      a$property_role <- row$property_role[[1]]
      a$property_code_requested <- row$property_code[[1]]
      a$depth_interval <- row$depth_interval[[1]]
      a$statistic <- row$statistic[[1]]
      attempts[[k]] <- a; k <- k + 1L
    }
    if (pause_seconds > 0) Sys.sleep(pause_seconds)
  }
  values_df <- dplyr::bind_rows(values)
  attempts_df <- dplyr::bind_rows(attempts)
  write_csv_utf8(values_df, file.path(output_dir, "soilgrids_point_values.csv"))
  write_csv_utf8(attempts_df, file.path(output_dir, "soilgrids_access_attempts.csv"))
  # Backward-compatible diagnostic name
  write_csv_utf8(attempts_df, file.path(output_dir, "soilgrids_url_attempts.csv"))
  missing_required <- values_df$required & !is.finite(values_df$converted_value)
  missing_optional <- !values_df$required & !is.finite(values_df$converted_value)
  if (any(missing_optional)) {
    write_csv_utf8(values_df[missing_optional, , drop = FALSE],
                   file.path(output_dir, "soilgrids_optional_layers_unavailable.csv"))
  }
  summary <- data.frame(
    requested_layers = nrow(values_df),
    extracted_layers = sum(is.finite(values_df$converted_value)),
    required_layers = sum(values_df$required),
    extracted_required_layers = sum(values_df$required & is.finite(values_df$converted_value)),
    optional_layers = sum(!values_df$required),
    extracted_optional_layers = sum(!values_df$required & is.finite(values_df$converted_value)),
    wcs_layers = sum(values_df$access_method == "WCS_GETCOVERAGE", na.rm = TRUE),
    webdav_layers = sum(values_df$access_method == "WEBDAV_VRT", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  write_csv_utf8(summary, file.path(output_dir, "soilgrids_access_summary.csv"))
  if (any(missing_required)) {
    write_csv_utf8(values_df[missing_required, , drop = FALSE], file.path(output_dir, "soilgrids_missing_required_layers.csv"))
    stop("Required SoilGrids layers could not be extracted through WCS or WebDAV. See soilgrids_access_attempts.csv and soilgrids_missing_required_layers.csv.", call. = FALSE)
  }
  values_df
}
