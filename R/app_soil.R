prepare_soil_input <- function(mode, profile_row, latitude, longitude, app_config,
                               reference_soil_id = "medium_silt",
                               local_theta_fc = NA_real_, local_theta_wp = NA_real_,
                               local_verified = FALSE,
                               run_dir = NULL) {
  max_root_depth <- as.numeric(profile_row$zr_scheduling_m[[1]])
  if (!is.finite(max_root_depth) || max_root_depth <= 0) {
    stop("Crop scheduling root depth is invalid.", call. = FALSE)
  }

  if (mode == "fao_reference") {
    refs <- load_soil_profiles()
    row <- get_soil_profile(refs, reference_soil_id)
    layers <- make_uniform_soil_layers(
      row$theta_fc[[1]], row$theta_wp[[1]], max_depth_m = max(2, max_root_depth),
      soil_scenario = "reference",
      source_type = "PUBLISHED_REFERENCE",
      source_note = paste0("Uniform FAO reference profile: ", row$soil_name_en[[1]])
    )
    return(list(
      layers = layers,
      source_class = "PUBLISHED_REFERENCE",
      source_label = paste0("FAO reference soil — ", row$soil_name_en[[1]]),
      run_dir = NULL,
      warnings = as.character(row$warning_es[[1]]),
      local_verified = FALSE
    ))
  }

  if (mode == "archived_santul") {
    demo_lat <- as.numeric(app_config$scientific_demo_latitude)
    demo_lon <- as.numeric(app_config$scientific_demo_longitude)
    distance_km <- sqrt((as.numeric(latitude) - demo_lat)^2 +
                          ((as.numeric(longitude) - demo_lon) * cos(demo_lat * pi / 180))^2) * 111.32
    if (!is.finite(distance_km) || distance_km > 1) {
      stop(
        "The saved SoilGrids reference is location-specific to the Santul demonstration point. ",
        "Select the Santul point, use a live SoilGrids query, or supply a local/reference profile.",
        call. = FALSE
      )
    }
    layers <- load_archived_santul_soil_layers()
    if (max(layers$depth_bottom_m) + 1e-9 < max_root_depth) {
      stop(
        "Archived Santul layers reach ", max(layers$depth_bottom_m),
        " m but the selected crop requires ", max_root_depth, " m.",
        call. = FALSE
      )
    }
    return(list(
      layers = layers,
      source_class = "PUBLIC_MODELLED",
      source_label = "Archived SoilGrids 2.0 matched-quantile profiles at the Santul demonstration point",
      run_dir = NULL,
      warnings = c(
        "SoilGrids is a 250 m global predictive product and is not a parcel measurement.",
        "Low/central/high matched quantiles are sensitivity profiles, not joint probabilistic confidence intervals."
      ),
      local_verified = FALSE
    ))
  }

  if (mode == "local_uniform") {
    source_class <- if (isTRUE(local_verified)) "LOCAL_VERIFIED" else "USER_DECLARED"
    source_type <- if (isTRUE(local_verified)) "LOCAL_VERIFIED_OVERRIDE" else "USER_DECLARED_LOCAL_PROFILE"
    scenario <- if (isTRUE(local_verified)) "local_verified" else "local_declared"
    layers <- make_uniform_soil_layers(
      local_theta_fc, local_theta_wp, max_depth_m = max(2, max_root_depth),
      soil_scenario = scenario,
      source_type = source_type,
      source_note = if (isTRUE(local_verified)) {
        "User-entered local values explicitly marked as verified/documented"
      } else {
        "User-declared local values; not relabelled as verified measurements"
      }
    )
    return(list(
      layers = layers,
      source_class = source_class,
      source_label = if (isTRUE(local_verified)) "Verified local uniform soil profile" else "User-declared local uniform soil profile",
      run_dir = NULL,
      warnings = if (isTRUE(local_verified)) character(0) else "Local soil values were declared but not verified.",
      local_verified = isTRUE(local_verified)
    ))
  }

  if (mode == "live_soilgrids") {
    if (!is.finite(as.numeric(latitude)) || !is.finite(as.numeric(longitude))) {
      stop("A productive location is required for live SoilGrids.", call. = FALSE)
    }
    run_dir <- run_dir %||% tempfile("riegoazuay_soilgrids_")
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    register <- as.data.frame(read_csv_utf8(file.path("config", "soilgrids_layer_register.csv")), stringsAsFactors = FALSE)
    order <- trimws(unlist(strsplit(as.character(app_config$soilgrids_access_order), ";", fixed = TRUE)))
    values <- query_soilgrids_register(
      register = register,
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      webdav_base_url = as.character(app_config$soilgrids_webdav_base_url),
      wcs_base_url = as.character(app_config$soilgrids_wcs_base_url),
      projection = as.character(app_config$soilgrids_projection),
      output_dir = run_dir,
      access_order = order,
      wcs_version = as.character(app_config$soilgrids_wcs_version),
      wcs_half_size_m = as.numeric(app_config$soilgrids_wcs_half_size_m),
      wcs_crs_uri = as.character(app_config$soilgrids_wcs_crs_uri),
      timeout_seconds = as.numeric(app_config$soilgrids_timeout_seconds),
      pause_seconds = as.numeric(app_config$soilgrids_request_pause_seconds)
    )
    layers <- build_soil_layer_scenarios(values, max_root_depth_m = max_root_depth)
    write_csv_utf8(layers, file.path(run_dir, "soil_layer_profiles.csv"))
    validity <- attr(layers, "validity_audit")
    if (!is.null(validity)) write_csv_utf8(validity, file.path(run_dir, "soil_profile_validity_audit.csv"))
    return(list(
      layers = layers,
      source_class = "PUBLIC_MODELLED",
      source_label = "Live SoilGrids 2.0 matched-quantile profiles at the productive point",
      run_dir = run_dir,
      warnings = c(
        "SoilGrids is a global predictive reference and does not substitute for a local soil analysis.",
        "Matched quantiles are treated as storage-sensitivity cases, not joint probabilistic intervals."
      ),
      local_verified = FALSE,
      raw_values = values
    ))
  }

  stop("Unknown soil mode: ", mode, call. = FALSE)
}
