validate_planning_inputs <- function(inputs) {
  required_text <- c("system_public_id", "crop_profile_id", "weather_mode", "soil_mode",
                     "flow_source", "initial_mode", "subsoil_mode", "productive_location_source",
                     "area_source", "crop_area_scope")
  for (nm in required_text) {
    if (!text_present(inputs[[nm]])) stop("Missing required input: ", nm, call. = FALSE)
  }
  required_numeric <- c("productive_latitude", "productive_longitude", "area_ha",
                        "delivery_efficiency", "turn_hours", "flow_lps")
  for (nm in required_numeric) {
    value <- suppressWarnings(as.numeric(inputs[[nm]]))
    if (!is.finite(value)) stop("Invalid numeric input: ", nm, call. = FALSE)
  }
  if (as.numeric(inputs$area_ha) <= 0) stop("Crop-specific planted area must be positive.", call. = FALSE)
  if (!safe_character(inputs$area_source) %in% c("declared", "polygon")) {
    stop("Area source must be declared or polygon.", call. = FALSE)
  }
  if (!identical(safe_character(inputs$crop_area_scope), "single_crop_homogeneous_zone")) {
    stop("This release requires one crop-specific homogeneous zone per calculation.", call. = FALSE)
  }
  if (identical(safe_character(inputs$area_source), "polygon")) {
    polygon_area <- suppressWarnings(as.numeric(inputs$polygon_area_ha))
    if (!is.finite(polygon_area) || polygon_area <= 0) {
      stop("Polygon-derived area was selected but no valid polygon area is available.", call. = FALSE)
    }
    if (abs(polygon_area - as.numeric(inputs$area_ha)) > 1e-8) {
      stop("Active area does not match the selected polygon-derived area.", call. = FALSE)
    }
  }
  if (as.numeric(inputs$delivery_efficiency) <= 0 || as.numeric(inputs$delivery_efficiency) > 1) {
    stop("Delivery/application efficiency must be in (0,1].", call. = FALSE)
  }
  if (as.numeric(inputs$turn_hours) < 0) stop("Turn hours cannot be negative.", call. = FALSE)
  if (as.numeric(inputs$flow_lps) <= 0) stop("Flow must be positive.", call. = FALSE)
  current_turn <- as.Date(inputs$current_turn_date)
  next_turn <- as.Date(inputs$next_turn_date)
  if (is.na(current_turn) || is.na(next_turn) || next_turn <= current_turn) {
    stop("The next turn must occur after the current turn.", call. = FALSE)
  }
  horizon_days <- as.integer(next_turn - current_turn)
  if (horizon_days > 15L) {
    stop("This build supports a maximum interval of 15 days between turns.", call. = FALSE)
  }
  runoff <- suppressWarnings(as.numeric(inputs$runoff_fraction %||% 0))
  if (!is.finite(runoff) || runoff < 0 || runoff >= 1) stop("Runoff fraction must be in [0,1).", call. = FALSE)
  invisible(TRUE)
}

prepare_weather_scenarios_for_engine <- function(weather_bundle, profile_row, central_soil,
                                                 crop_age, initial_depletion_mm,
                                                 inputs, start_date, end_date) {
  selection <- NULL
  distribution <- NULL
  member_summary <- NULL
  if (!is.null(weather_bundle$member_daily)) {
    member_daily <- as.data.frame(weather_bundle$member_daily, stringsAsFactors = FALSE)
    member_daily$weather_scenario <- as.character(member_daily$member_id)
    hours_map <- stats::setNames(as.numeric(inputs$turn_hours), as.character(start_date))
    next_map <- stats::setNames(as.character(as.Date(inputs$next_turn_date)), as.character(start_date))
    member_plans <- plan_weather_ensemble_layered(
      weather_ensemble = member_daily,
      profile_row = profile_row,
      soil_layers = central_soil,
      days_after_planting_start = crop_age,
      initial_depletion_mm = initial_depletion_mm,
      area_ha = as.numeric(inputs$area_ha),
      delivery_efficiency = as.numeric(inputs$delivery_efficiency),
      allowed_dates = start_date,
      flow_lps = as.numeric(inputs$flow_lps),
      max_hours_by_date = hours_map,
      next_turn_by_date = next_map,
      reporting_horizon_end_date = end_date,
      adjust_p_for_etc = TRUE,
      runoff_fraction = as.numeric(inputs$runoff_fraction),
      subsoil_water_mode = as.character(inputs$subsoil_mode),
      subsoil_theta_value = as.numeric(inputs$subsoil_theta_value %||% NA_real_)
    )
    chosen <- select_member_scenarios_by_irrigation(
      member_daily = member_daily,
      member_plan_summary = member_plans$scenario_summary,
      validation_start = start_date,
      validation_end = end_date
    )
    weather <- chosen$selected_daily
    selection <- chosen$selection
    distribution <- chosen$distribution
    member_summary <- chosen$member_plan_summary
  } else {
    weather <- as.data.frame(weather_bundle$selected_weather, stringsAsFactors = FALSE)
  }
  weather$weather_scenario <- normalise_weather_scenario_labels(weather$weather_scenario)
  scenarios <- unique(weather$weather_scenario)
  if (length(scenarios) == 1L) weather$weather_scenario <- "central"
  if (length(unique(weather$weather_scenario)) > 1L && !"central" %in% unique(weather$weather_scenario)) {
    stop("Multiple weather scenarios require one scenario labelled 'central'.", call. = FALSE)
  }
  weather$date <- as.Date(weather$date)
  weather$eto_mm_day <- as.numeric(weather$eto_mm_day)
  weather$precip_mm_day <- as.numeric(weather$precip_mm_day)
  expected <- seq(start_date, end_date, by = "day")
  for (s in unique(weather$weather_scenario)) {
    z <- weather[weather$weather_scenario == s, , drop = FALSE]
    if (anyDuplicated(z$date)) stop("Weather scenario contains duplicated dates: ", s, call. = FALSE)
    missing_dates <- setdiff(expected, z$date)
    if (length(missing_dates)) stop("Weather scenario is incomplete: ", s, call. = FALSE)
  }
  list(
    weather = weather,
    selection = selection,
    distribution = distribution,
    member_summary = member_summary
  )
}

choose_central_soil_scenario <- function(layers) {
  scenarios <- unique(as.character(layers$soil_scenario))
  if ("central" %in% scenarios) return("central")
  scenarios[[1]]
}

assess_flow_context <- function(inputs) {
  measured_sources <- c("meter", "volumetric", "calibrated_structure", "other_documented")
  source <- safe_character(inputs$flow_source)
  measured_method <- source %in% measured_sources
  measurement_date <- suppressWarnings(as.Date(safe_character(inputs$flow_measurement_date)))
  date_valid <- measured_method && !is.na(measurement_date) && measurement_date <= Sys.Date()
  location_valid <- measured_method && text_present(inputs$flow_measurement_location)
  detail_valid <- measured_method && text_present(inputs$flow_measurement_method_detail)
  repetitions <- suppressWarnings(as.integer(inputs$flow_repetition_count %||% 0))
  minimum_repetitions <- if (identical(source, "volumetric")) 2L else 1L
  repetitions_valid <- measured_method && is.finite(repetitions) && repetitions >= minimum_repetitions
  context_documented <- measured_method && isTRUE(inputs$flow_context_documented)
  context_complete <- measured_method && date_valid && location_valid && detail_valid &&
    repetitions_valid && context_documented
  requested_verified <- isTRUE(inputs$flow_verified)
  accepted_verified <- requested_verified && context_complete
  missing <- character(0)
  if (measured_method) {
    if (!date_valid) missing <- c(missing, "measurement_date")
    if (!location_valid) missing <- c(missing, "measurement_location")
    if (!detail_valid) missing <- c(missing, "method_or_instrument")
    if (!repetitions_valid) missing <- c(missing, paste0("minimum_repetitions_", minimum_repetitions))
    if (!context_documented) missing <- c(missing, "documented_context_confirmation")
  }
  list(
    source = source,
    measured_method = measured_method,
    measurement_date = if (is.na(measurement_date)) NA_character_ else as.character(measurement_date),
    location = safe_character(inputs$flow_measurement_location),
    method_detail = safe_character(inputs$flow_measurement_method_detail),
    repetitions = repetitions,
    minimum_repetitions = minimum_repetitions,
    context_documented = context_documented,
    context_complete = context_complete,
    requested_verified = requested_verified,
    accepted_verified = accepted_verified,
    missing_fields = missing
  )
}

build_provenance_table <- function(inputs, registry_row, profile_row, weather_bundle,
                                   soil_bundle, evidence_level, crop_age) {
  flow_context <- assess_flow_context(inputs)
  flow_class <- if (isTRUE(flow_context$accepted_verified)) {
    "LOCAL_VERIFIED"
  } else if (identical(inputs$flow_source, "institutional")) {
    "INSTITUTIONAL_REFERENCE"
  } else {
    "USER_DECLARED"
  }
  context_class <- if (isTRUE(flow_context$accepted_verified)) {
    "LOCAL_VERIFIED"
  } else if (isTRUE(flow_context$measured_method)) {
    "USER_DECLARED"
  } else {
    "MISSING"
  }
  initial_class <- if (isTRUE(inputs$initial_water_verified)) "LOCAL_VERIFIED" else "USER_DECLARED"
  subsoil_class <- if (isTRUE(inputs$subsoil_verified) && identical(inputs$subsoil_mode, "measured_theta")) {
    "LOCAL_VERIFIED"
  } else if (identical(inputs$subsoil_mode, "measured_theta")) {
    "USER_DECLARED"
  } else {
    "PUBLISHED_REFERENCE"
  }
  turn_class <- if (isTRUE(inputs$turn_schedule_verified)) "LOCAL_VERIFIED" else "USER_DECLARED"
  history_status <- safe_character(inputs$recent_irrigation_history_status, "unavailable")
  history_class <- if (identical(history_status, "documented")) {
    "LOCAL_VERIFIED"
  } else if (identical(history_status, "declared")) {
    "USER_DECLARED"
  } else {
    "MISSING"
  }
  flow_context_value <- if (isTRUE(flow_context$measured_method)) {
    paste0(
      "date=", flow_context$measurement_date, "; location=", flow_context$location,
      "; method=", flow_context$method_detail, "; repetitions=", flow_context$repetitions,
      "; context_complete=", flow_context$context_complete
    )
  } else {
    "not_applicable_to_reference_or_declared_flow"
  }
  data.frame(
    input_group = c(
      "territorial_registry", "institutional_intake", "productive_location", "crop_profile",
      "crop_age_or_planting_date", "productive_area", "delivery_efficiency", "weather",
      "soil", "initial_root_zone_water", "active_flow", "flow_measurement_context",
      "turn_schedule", "recent_irrigation_history", "subsoil_water", "readiness"
    ),
    active_value = c(
      paste0(registry_row$public_id[[1]], " | ", registry_row$system_name[[1]]),
      paste(registry_row$latitude[[1]], registry_row$longitude[[1]], sep = ", "),
      paste(inputs$productive_latitude, inputs$productive_longitude, sep = ", "),
      paste0(profile_row$crop_profile_id[[1]], " | ", profile_row$scientific_name[[1]]),
      as.character(crop_age),
      paste0(inputs$area_ha, " ha; source=", inputs$area_source,
             "; declared=", inputs$declared_area_ha,
             "; polygon=", inputs$polygon_area_ha,
             "; scope=", inputs$crop_area_scope),
      as.character(inputs$delivery_efficiency),
      weather_bundle$source_label,
      soil_bundle$source_label,
      paste0(inputs$initial_mode, " = ", inputs$initial_value),
      paste0(inputs$flow_lps, " L/s; ", inputs$flow_source),
      flow_context_value,
      paste0(inputs$current_turn_date, " -> ", inputs$next_turn_date, "; ", inputs$turn_hours,
             " h; verified=", isTRUE(inputs$turn_schedule_verified)),
      history_status,
      as.character(inputs$subsoil_mode),
      evidence_level
    ),
    provenance_class = c(
      "INSTITUTIONAL_REFERENCE", "INSTITUTIONAL_REFERENCE",
      if (safe_character(inputs$productive_location_source) %in% c("map_click", "manual", "polygon_centroid")) "USER_DECLARED" else "INSTITUTIONAL_REFERENCE",
      "PUBLISHED_REFERENCE", "USER_DECLARED", "USER_DECLARED", "USER_DECLARED",
      weather_bundle$source_class, soil_bundle$source_class, initial_class, flow_class,
      context_class, turn_class, history_class, subsoil_class, "INTERNAL_ENGINEERING_RULE"
    ),
    verified = c(
      TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
      weather_bundle$source_class == "LOCAL_VERIFIED", soil_bundle$source_class == "LOCAL_VERIFIED",
      isTRUE(inputs$initial_water_verified), isTRUE(flow_context$accepted_verified),
      isTRUE(flow_context$accepted_verified), isTRUE(inputs$turn_schedule_verified),
      identical(history_status, "documented"), subsoil_class == "LOCAL_VERIFIED", NA
    ),
    calculation_role = c(
      "selector_context", "context_only", "weather_and_soil_query_point", "crop_parameters",
      "phenological_state", "volume_conversion", "gross_to_net_conversion", "daily_balance",
      "root_zone_storage", "initial_depletion", "turn_capacity", "flow_evidence_gate",
      "turn_timing_and_evidence_gate", "initial_state_interpretation", "root_expansion_assumption",
      "output_authority_gate"
    ),
    stringsAsFactors = FALSE
  )
}

readiness_details <- function(inputs, root_growth_occurs) {
  initial_evidence <- if (isTRUE(inputs$initial_water_verified)) {
    if (identical(inputs$initial_mode, "measured_theta")) "measured_theta" else "recent_full_wetting_verified"
  } else {
    "estimated"
  }
  flow_context <- assess_flow_context(inputs)
  flow_verified <- isTRUE(flow_context$accepted_verified)
  turn_verified <- isTRUE(inputs$turn_schedule_verified)
  history_status <- safe_character(inputs$recent_irrigation_history_status, "unavailable")
  history_available <- history_status %in% c("declared", "documented")
  subsoil_evidence <- if (!isTRUE(root_growth_occurs)) {
    "no_root_expansion"
  } else if (isTRUE(inputs$subsoil_verified) && identical(inputs$subsoil_mode, "measured_theta")) {
    "measured_theta"
  } else {
    "assumed"
  }
  level <- assess_evidence_level(
    has_crop = text_present(inputs$crop_profile_id),
    has_area = is.finite(as.numeric(inputs$area_ha)) && as.numeric(inputs$area_ha) > 0,
    has_stage_or_date = TRUE,
    has_soil = TRUE,
    has_weather = TRUE,
    initial_water_evidence = initial_evidence,
    flow_quality = safe_character(inputs$flow_source),
    flow_operational_verified = flow_verified,
    has_turn_schedule = turn_verified,
    community_aggregation = FALSE,
    subsoil_water_evidence = subsoil_evidence,
    has_recent_irrigation_history = history_available
  )
  list(
    level = level,
    initial_evidence = initial_evidence,
    flow_operational_verified = flow_verified,
    flow_context = flow_context,
    turn_schedule_verified = turn_verified,
    recent_irrigation_history_status = history_status,
    recent_irrigation_history_available = history_available,
    subsoil_water_evidence = subsoil_evidence,
    root_growth_occurs = root_growth_occurs
  )
}

build_bilingual_warnings <- function(inputs, weather_bundle, soil_bundle, readiness, root_growth_occurs) {
  rows <- list()
  add <- function(code, es, en) {
    rows[[length(rows) + 1L]] <<- data.frame(
      warning_code = code,
      message_es = es,
      message_en = en,
      stringsAsFactors = FALSE
    )
  }
  weather_mode <- safe_character(inputs$weather_mode)
  if (identical(weather_mode, "offline_fixture")) {
    add("WEATHER_SYNTHETIC", "Los datos meteorológicos son sintéticos y se usan únicamente para demostrar y probar la plataforma sin internet; no constituyen un pronóstico operativo.", "The weather data are synthetic and are used only to demonstrate and test the platform offline; they are not an operational forecast.")
  } else if (identical(weather_mode, "archived_fixture")) {
    add("WEATHER_ARCHIVED", "El pronóstico ECMWF archivado fue desplazado temporalmente para una prueba reproducible y no representa las condiciones actuales.", "The archived ECMWF forecast was time-shifted for reproducible software testing and does not represent current conditions.")
  } else if (identical(weather_mode, "manual_csv")) {
    add("WEATHER_USER", "La serie meteorológica fue suministrada por el usuario; su calidad y representatividad local deben documentarse por separado.", "The weather series was supplied by the user; its quality and local representativeness require separate documentation.")
  } else if (identical(weather_mode, "live_openmeteo")) {
    add("WEATHER_PUBLIC_MODEL", "El pronóstico ECMWF vía Open-Meteo es un producto público/modelado y no equivale a una observación meteorológica de la parcela.", "The ECMWF forecast via Open-Meteo is a public/modelled product and is not a parcel meteorological observation.")
  }
  if (length(weather_bundle$warnings) && any(nzchar(trimws(weather_bundle$warnings)))) {
    detail <- paste(weather_bundle$warnings[nzchar(trimws(weather_bundle$warnings))], collapse = " | ")
    add("WEATHER_GATEWAY_DETAIL", paste0("Detalle técnico del gateway meteorológico: ", detail), paste0("Weather-gateway technical detail: ", detail))
  }

  soil_mode <- safe_character(inputs$soil_mode)
  if (identical(soil_mode, "fao_reference")) {
    add("SOIL_REFERENCE", "El perfil de suelo es una referencia publicada uniforme; debe sustituirse por información local cuando esté disponible.", "The soil profile is a uniform published reference and should be replaced by local information when available.")
  } else if (soil_mode %in% c("archived_santul", "live_soilgrids")) {
    add("SOIL_PUBLIC_MODEL", "La información del suelo proviene de un mapa global de referencia y no de una medición realizada en la parcela.", "The soil information comes from a global reference map rather than a measurement taken in the parcel.")
    add("SOIL_QUANTILES", "Los niveles bajo, central y alto representan casos de sensibilidad del almacenamiento de agua; no son límites probabilísticos de la parcela.", "The low, central, and high levels are water-storage sensitivity cases; they are not probabilistic parcel limits.")
  } else if (identical(soil_mode, "local_uniform") && !isTRUE(inputs$local_soil_verified)) {
    add("SOIL_DECLARED", "Los parámetros locales de suelo fueron declarados pero no verificados documentalmente.", "The local soil parameters were declared but not documentarily verified.")
  }

  if (!isTRUE(readiness$flow_operational_verified)) {
    add("FLOW_UNVERIFIED", "El caudal activo no está verificado como una medición parcelaria actual con contexto completo.", "The active flow is not verified as a current parcel-level measurement with complete context.")
  }
  if (isTRUE(inputs$flow_verified) && !isTRUE(readiness$flow_operational_verified)) {
    missing <- paste(readiness$flow_context$missing_fields, collapse = ", ")
    add("FLOW_CONTEXT_INCOMPLETE", paste0("La solicitud de verificar el caudal no fue aceptada; faltan: ", missing, "."), paste0("The request to verify flow was not accepted; missing: ", missing, "."))
  }
  if (!isTRUE(readiness$turn_schedule_verified)) {
    add("TURN_UNVERIFIED", "El calendario declarado de turnos no ha sido marcado como verificado con la organización de riego.", "The declared turn schedule has not been marked as verified with the irrigation organisation.")
  }
  if (!isTRUE(readiness$recent_irrigation_history_available)) {
    add("HISTORY_MISSING", "No se dispone de historial reciente de riego; el estado inicial es menos defendible para uso operativo.", "Recent irrigation history is unavailable; the initial state is less defensible for operational use.")
  }
  if (!isTRUE(inputs$initial_water_verified)) {
    add("INITIAL_WATER_UNVERIFIED", "El estado hídrico inicial de la zona radicular es estimado o declarado, no verificado.", "Initial root-zone water status is estimated or declared rather than verified.")
  }
  if (isTRUE(root_growth_occurs) && !isTRUE(inputs$subsoil_verified)) {
    add("SUBSOIL_ASSUMED", "El estado hídrico del subsuelo recién explorado por las raíces se asume y no está verificado.", "Water status of newly explored subsoil is assumed rather than verified.")
  }
  if (identical(inputs$productive_location_source, "intake_approximation")) {
    add("INTAKE_APPROXIMATION", "La captación registrada se usa explícitamente como aproximación exploratoria de la ubicación del cultivo; no se interpreta como centro de la parcela.", "The registered intake is explicitly used as an exploratory approximation of the crop location; it is not interpreted as the parcel centre.")
  }
  add("CROP_AREA_SCOPE", "La superficie activa corresponde únicamente al terreno sembrado con el cultivo seleccionado; no representa toda la propiedad ni todo el sistema de riego.", "The active area corresponds only to land planted with the selected crop; it does not represent the whole property or irrigation system.")
  if (safe_character(inputs$productive_location_source) %in% c("map_click", "manual", "intake_approximation")) {
    add("POINT_NO_FOOTPRINT", "La ubicación puntual no crea una geometría ni distribuye la superficie alrededor del punto. El volumen usa el área numérica declarada.", "A point location does not create a footprint or distribute area around the point. Volume uses the declared numeric area.")
  }
  if (identical(safe_character(inputs$area_source), "polygon")) {
    add("POLYGON_AREA_USED", "El volumen se calculó con el área derivada del polígono que el usuario dibujó para el cultivo seleccionado.", "Volume was calculated using the area derived from the polygon drawn for the selected crop.")
  } else if (is.finite(suppressWarnings(as.numeric(inputs$polygon_area_ha)))) {
    add("DECLARED_AREA_USED_WITH_POLYGON", "Existe un polígono dibujado, pero el usuario eligió conservar el área declarada para el cálculo.", "A polygon was drawn, but the user chose to retain the declared area for the calculation.")
  }
  if (identical(readiness$level, "E2")) {
    add("READINESS_E2", "El plan es provisional y no constituye validación operativa en campo.", "The plan is provisional and does not constitute operational field validation.")
  }
  add("PROTOTYPE_BOUNDARY", "El prototipo no opera infraestructura, no modifica derechos de agua ni redistribuye agua de forma autónoma.", "The prototype does not operate infrastructure, alter water rights, or autonomously redistribute water.")
  dplyr::bind_rows(rows)
}

compute_riegoazuay_plan <- function(inputs, registry, app_config) {
  validate_planning_inputs(inputs)
  start_date <- as.Date(inputs$current_turn_date)
  end_date <- as.Date(inputs$next_turn_date) - 1
  registry_row <- selected_registry_row(registry, inputs$system_public_id)
  profiles <- load_crop_profiles_daily()
  profile <- get_crop_profile(profiles, inputs$crop_profile_id)
  crop_age <- crop_age_on_date(
    profile, inputs$crop_age_mode, inputs$planting_date,
    inputs$crop_age_days, start_date
  )
  horizon_days <- as.integer(end_date - start_date) + 1L
  if (crop_age + horizon_days - 1L > as.integer(profile$cycle_days[[1]])) {
    stop("The planning horizon extends beyond the configured crop cycle.", call. = FALSE)
  }

  run_id <- paste0("scenario_", format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ"), "_", sprintf("%04d", sample.int(9999, 1)))
  run_dir <- file.path("outputs", "results", run_id)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  soil_bundle <- prepare_soil_input(
    mode = inputs$soil_mode,
    profile_row = profile,
    latitude = inputs$productive_latitude,
    longitude = inputs$productive_longitude,
    app_config = app_config,
    reference_soil_id = inputs$reference_soil_id,
    local_theta_fc = inputs$local_theta_fc,
    local_theta_wp = inputs$local_theta_wp,
    local_verified = isTRUE(inputs$local_soil_verified),
    run_dir = file.path(run_dir, "soil")
  )
  soil_layers_all <- as.data.frame(soil_bundle$layers, stringsAsFactors = FALSE)
  central_soil_name <- choose_central_soil_scenario(soil_layers_all)
  central_soil <- soil_layers_all[soil_layers_all$soil_scenario == central_soil_name, , drop = FALSE]

  root_curve <- build_root_depth_curve(profile)
  kc_curve <- build_kc_curve(profile)
  root_depth_start <- root_curve[[crop_age]]
  root_depth_end <- root_curve[[crop_age + horizon_days - 1L]]
  root_growth_occurs <- root_depth_end > root_depth_start + 1e-9

  weather_bundle <- prepare_weather_input(
    mode = inputs$weather_mode,
    start_date = start_date,
    end_date = end_date,
    upload_path = inputs$weather_upload_path,
    latitude = inputs$productive_latitude,
    longitude = inputs$productive_longitude,
    elevation_m = inputs$productive_elevation_m,
    app_config = app_config,
    run_dir = file.path(run_dir, "weather")
  )

  central_initial <- initial_depletion_layered(
    inputs$initial_mode, inputs$initial_value, central_soil, root_depth_start,
    p_table = profile$p_table[[1]],
    etc_potential_mm_day = 5
  )
  weather_prepared <- prepare_weather_scenarios_for_engine(
    weather_bundle, profile, central_soil, crop_age, central_initial,
    inputs, start_date, end_date
  )
  weather <- weather_prepared$weather

  hours_map <- stats::setNames(as.numeric(inputs$turn_hours), as.character(start_date))
  next_map <- stats::setNames(as.character(as.Date(inputs$next_turn_date)), as.character(start_date))
  scenario_summaries <- list()
  all_daily <- list()
  all_events <- list()
  initial_rows <- list()
  plan_objects <- list()
  soil_scenarios <- unique(as.character(soil_layers_all$soil_scenario))

  for (soil_name in soil_scenarios) {
    layers <- soil_layers_all[soil_layers_all$soil_scenario == soil_name, , drop = FALSE]
    first_central_weather <- weather[weather$weather_scenario == "central", , drop = FALSE]
    if (!nrow(first_central_weather)) first_central_weather <- weather[1, , drop = FALSE]
    etc_first <- first_central_weather$eto_mm_day[[1]] * kc_curve[[crop_age]]
    initial_depletion <- initial_depletion_layered(
      inputs$initial_mode, inputs$initial_value, layers, root_depth_start,
      p_table = profile$p_table[[1]], etc_potential_mm_day = etc_first
    )
    initial_rows[[soil_name]] <- data.frame(
      soil_scenario = soil_name,
      root_depth_start_m = root_depth_start,
      initial_depletion_mm = initial_depletion,
      initial_mode = inputs$initial_mode,
      initial_value = as.numeric(inputs$initial_value),
      stringsAsFactors = FALSE
    )
    plans <- plan_weather_ensemble_layered(
      weather_ensemble = weather,
      profile_row = profile,
      soil_layers = layers,
      days_after_planting_start = crop_age,
      initial_depletion_mm = initial_depletion,
      area_ha = as.numeric(inputs$area_ha),
      delivery_efficiency = as.numeric(inputs$delivery_efficiency),
      allowed_dates = start_date,
      flow_lps = as.numeric(inputs$flow_lps),
      max_hours_by_date = hours_map,
      next_turn_by_date = next_map,
      reporting_horizon_end_date = end_date,
      adjust_p_for_etc = TRUE,
      runoff_fraction = as.numeric(inputs$runoff_fraction),
      subsoil_water_mode = as.character(inputs$subsoil_mode),
      subsoil_theta_value = as.numeric(inputs$subsoil_theta_value %||% NA_real_)
    )
    summary <- plans$scenario_summary
    summary$soil_scenario <- soil_name
    scenario_summaries[[soil_name]] <- summary
    plan_objects[[soil_name]] <- plans
    for (weather_name in names(plans$plans)) {
      z <- plans$plans[[weather_name]]
      z$daily$soil_scenario <- soil_name
      z$daily$weather_scenario <- weather_name
      if (nrow(z$events)) {
        z$events$soil_scenario <- soil_name
        z$events$weather_scenario <- weather_name
      }
      all_daily[[paste(soil_name, weather_name, sep = "::")]] <- z$daily
      all_events[[paste(soil_name, weather_name, sep = "::")]] <- z$events
    }
  }

  scenario_summary <- dplyr::bind_rows(scenario_summaries)
  daily_all <- dplyr::bind_rows(all_daily)
  events_all <- dplyr::bind_rows(all_events)
  initial_summary <- dplyr::bind_rows(initial_rows)

  central_rows <- scenario_summary[
    scenario_summary$soil_scenario == central_soil_name &
      scenario_summary$weather_scenario == "central", , drop = FALSE
  ]
  if (nrow(central_rows) != 1L) {
    central_rows <- scenario_summary[scenario_summary$soil_scenario == central_soil_name, , drop = FALSE]
    central_rows <- central_rows[1, , drop = FALSE]
  }
  central_weather_name <- as.character(central_rows$weather_scenario[[1]])
  central_daily <- daily_all[
    daily_all$soil_scenario == central_soil_name & daily_all$weather_scenario == central_weather_name,
    , drop = FALSE
  ]
  central_events <- events_all[
    events_all$soil_scenario == central_soil_name & events_all$weather_scenario == central_weather_name,
    , drop = FALSE
  ]

  readiness <- readiness_details(inputs, root_growth_occurs)
  provenance <- build_provenance_table(
    inputs, registry_row, profile, weather_bundle, soil_bundle, readiness$level, crop_age
  )

  required_min <- min(scenario_summary$required_volume_m3, na.rm = TRUE)
  required_max <- max(scenario_summary$required_volume_m3, na.rm = TRUE)
  envelope <- data.frame(
    central_soil_scenario = central_soil_name,
    central_weather_scenario = central_weather_name,
    central_required_volume_m3 = central_rows$required_volume_m3[[1]],
    central_deliverable_volume_m3 = central_rows$deliverable_volume_m3[[1]],
    central_unmet_volume_m3 = central_rows$unmet_volume_m3[[1]],
    central_required_hours = central_rows$required_hours[[1]],
    central_turn_capacity_m3 = if (nrow(central_events)) central_events$turn_capacity_m3[[1]] else NA_real_,
    minimum_required_volume_m3 = required_min,
    maximum_required_volume_m3 = required_max,
    scenario_count = nrow(scenario_summary),
    readiness_level = readiness$level,
    stringsAsFactors = FALSE
  )

  warnings_table <- build_bilingual_warnings(
    inputs = inputs,
    weather_bundle = weather_bundle,
    soil_bundle = soil_bundle,
    readiness = readiness,
    root_growth_occurs = root_growth_occurs
  )
  warnings_es <- unique(warnings_table$message_es[nzchar(trimws(warnings_table$message_es))])
  warnings_en <- unique(warnings_table$message_en[nzchar(trimws(warnings_table$message_en))])


  max_residual <- if (nrow(daily_all)) max(abs(daily_all$balance_residual_mm), na.rm = TRUE) else NA_real_
  diagnostics <- data.frame(
    diagnostic = c(
      "maximum_absolute_mass_balance_residual_mm", "weather_scenarios", "soil_scenarios",
      "combined_scenarios", "daily_rows", "turn_event_rows", "root_growth_occurs",
      "root_depth_start_m", "root_depth_end_m", "flow_context_complete",
      "flow_operational_verified", "turn_schedule_verified", "recent_irrigation_history_status",
      "crop_area_scope", "area_source", "effective_crop_area_ha", "declared_crop_area_ha", "polygon_area_ha"
    ),
    value = c(
      max_residual, length(unique(weather$weather_scenario)), length(soil_scenarios),
      nrow(scenario_summary), nrow(daily_all), nrow(events_all), root_growth_occurs,
      root_depth_start, root_depth_end, readiness$flow_context$context_complete,
      readiness$flow_operational_verified, readiness$turn_schedule_verified,
      readiness$recent_irrigation_history_status,
      safe_character(inputs$crop_area_scope), safe_character(inputs$area_source),
      as.numeric(inputs$area_ha), as.numeric(inputs$declared_area_ha), as.numeric(inputs$polygon_area_ha)
    ),
    stringsAsFactors = FALSE
  )

  input_snapshot <- data.frame(
    parameter = names(inputs),
    value = vapply(inputs, function(x) paste(as.character(x), collapse = ";"), character(1)),
    stringsAsFactors = FALSE
  )

  result <- list(
    run_id = run_id,
    software_version = package_version_string(),
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    registry_row = registry_row,
    profile = profile,
    crop_age_days = crop_age,
    crop_stage_es = crop_stage_name(profile, crop_age, "es"),
    crop_stage_en = crop_stage_name(profile, crop_age, "en"),
    soil_layers = soil_layers_all,
    soil_root_zone_summary = summarise_root_zone_profiles(soil_layers_all, root_depth_start, profile$p_table[[1]]),
    initial_summary = initial_summary,
    weather = weather,
    weather_selection = weather_prepared$selection,
    weather_distribution = weather_prepared$distribution,
    weather_member_summary = weather_prepared$member_summary,
    scenario_summary = scenario_summary,
    central_summary = central_rows,
    envelope = envelope,
    daily_all = daily_all,
    events_all = events_all,
    central_daily = central_daily,
    central_events = central_events,
    readiness = readiness,
    flow_context = readiness$flow_context,
    provenance = provenance,
    warnings = warnings_en,
    warnings_es = warnings_es,
    warnings_en = warnings_en,
    warnings_table = warnings_table,
    diagnostics = diagnostics,
    inputs = inputs,
    input_snapshot = input_snapshot,
    run_dir = run_dir,
    external_run_dirs = list(weather = weather_bundle$run_dir, soil = soil_bundle$run_dir)
  )

  write_csv_utf8(input_snapshot, file.path(run_dir, "input_snapshot.csv"))
  write_csv_utf8(scenario_summary, file.path(run_dir, "scenario_summary.csv"))
  write_csv_utf8(central_daily, file.path(run_dir, "central_daily_balance.csv"))
  write_csv_utf8(central_events, file.path(run_dir, "central_turn_events.csv"))
  write_csv_utf8(provenance, file.path(run_dir, "input_provenance.csv"))
  write_csv_utf8(warnings_table, file.path(run_dir, "warnings_bilingual.csv"))
  writeLines(warnings_es, file.path(run_dir, "warnings_es.txt"), useBytes = TRUE)
  writeLines(warnings_en, file.path(run_dir, "warnings_en.txt"), useBytes = TRUE)
  write_csv_utf8(diagnostics, file.path(run_dir, "diagnostics.csv"))
  saveRDS(result, file.path(run_dir, "scenario_result.rds"))
  result
}
