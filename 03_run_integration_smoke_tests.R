source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "profile_library.R"), encoding = "UTF-8")
source(file.path("R", "fao56_daily_balance.R"), encoding = "UTF-8")
source(file.path("R", "layered_soil_balance.R"), encoding = "UTF-8")
source(file.path("R", "irrigation_scheduler.R"), encoding = "UTF-8")
source(file.path("R", "layered_irrigation_scheduler.R"), encoding = "UTF-8")
source(file.path("R", "evidence_gate.R"), encoding = "UTF-8")
source(file.path("R", "flow_measurement_assistant.R"), encoding = "UTF-8")
source(file.path("R", "forecast_archive.R"), encoding = "UTF-8")
source(file.path("R", "soilgrids_gateway.R"), encoding = "UTF-8")
source(file.path("R", "soil_profile_integration.R"), encoding = "UTF-8")
source(file.path("R", "app_data.R"), encoding = "UTF-8")
source(file.path("R", "app_weather.R"), encoding = "UTF-8")
source(file.path("R", "app_soil.R"), encoding = "UTF-8")
source(file.path("R", "app_engine.R"), encoding = "UTF-8")
source(file.path("R", "app_reporting.R"), encoding = "UTF-8")
source(file.path("R", "app_i18n.R"), encoding = "UTF-8")

cat("\n=== RIEGOAZUAY DSS: INTEGRATION SMOKE TEST ===\n")
app_config <- load_app_config()
registry <- load_system_registry(app_config)
registry_mode <- attr(registry, "registry_mode")
restricted_mode <- identical(registry_mode, "restricted")
configured_demo_id <- trimws(as.character(app_config$scientific_demo_system_id))
selected_id <- if (restricted_mode && configured_demo_id %in% registry$public_id) configured_demo_id else registry$public_id[[1]]
selected_admin_row <- selected_registry_row(registry, selected_id)
selected_admin_canton <- registry_text_or_blank(selected_admin_row$canton_display[[1]])
selected_admin_parish <- registry_text_or_blank(selected_admin_row$parish_display[[1]])
selected_admin_parish_choices <- registry_filter_choices(
  filter_system_registry(registry, canton = selected_admin_canton, parish = "__ALL__")$parish_display
)
selected_row <- selected_registry_row(registry, selected_id)
productive_latitude <- if (restricted_mode) as.numeric(app_config$scientific_demo_latitude) else selected_row$latitude[[1]]
productive_longitude <- if (restricted_mode) as.numeric(app_config$scientific_demo_longitude) else selected_row$longitude[[1]]
productive_elevation <- if (restricted_mode) as.numeric(app_config$scientific_demo_elevation_m) else if (is.finite(selected_row$altitude_m[[1]])) selected_row$altitude_m[[1]] else 2600
soil_mode <- if (restricted_mode) "archived_santul" else "fao_reference"
expected_registry_n <- if (restricted_mode) 159L else nrow(registry)
expected_soil_n <- if (restricted_mode) 3L else 1L
expected_combined_n <- expected_soil_n * 3L

inputs <- list(
  system_public_id = selected_id,
  productive_latitude = productive_latitude,
  productive_longitude = productive_longitude,
  productive_elevation_m = productive_elevation,
  productive_location_source = "manual",
  productive_polygon_geojson = "",
  polygon_area_ha = NA_real_,
  polygon_centroid_latitude = NA_real_,
  polygon_centroid_longitude = NA_real_,
  area_source = "declared",
  declared_area_ha = 0.25,
  crop_area_scope = "single_crop_homogeneous_zone",
  crop_profile_id = "papa_superchola",
  crop_age_mode = "crop_age",
  planting_date = "2026-09-02",
  crop_age_days = 60,
  area_ha = 0.25,
  delivery_efficiency = 0.75,
  current_turn_date = "2026-11-01",
  next_turn_date = "2026-11-08",
  turn_hours = 6,
  flow_lps = 2,
  flow_source = "declared",
  flow_verified = FALSE,
  flow_measurement_date = "",
  flow_measurement_location = "",
  flow_measurement_method_detail = "",
  flow_repetition_count = 0,
  flow_context_documented = FALSE,
  turn_schedule_verified = FALSE,
  recent_irrigation_history_status = "unavailable",
  weather_mode = "offline_fixture",
  weather_upload_path = NULL,
  soil_mode = soil_mode,
  reference_soil_id = "medium_silt",
  local_theta_fc = NA_real_,
  local_theta_wp = NA_real_,
  local_soil_verified = FALSE,
  initial_mode = "fraction_raw",
  initial_value = 0.90,
  initial_water_verified = FALSE,
  subsoil_mode = "field_capacity",
  subsoil_theta_value = NA_real_,
  subsoil_verified = FALSE,
  runoff_fraction = 0
)

result <- compute_riegoazuay_plan(inputs, registry, app_config)
smoke_dir <- file.path("outputs", "smoke")
dir.create(smoke_dir, recursive = TRUE, showWarnings = FALSE)
html_path <- file.path(smoke_dir, "RiegoAzuay_DSS_smoke_report_ES_EN.html")
csv_path <- file.path(smoke_dir, "RiegoAzuay_DSS_smoke_results.csv")
xlsx_es_path <- file.path(smoke_dir, "RiegoAzuay_DSS_smoke_plan_ES.xlsx")
xlsx_en_path <- file.path(smoke_dir, "RiegoAzuay_DSS_smoke_plan_EN.xlsx")
rds_path <- file.path(smoke_dir, "RiegoAzuay_DSS_smoke_result.rds")
zip_path <- file.path(smoke_dir, "RiegoAzuay_DSS_smoke_reproducibility_bundle.zip")
write_bilingual_html_report(result, html_path)
write_result_csv_export(result, csv_path)
write_user_xlsx_export(result, xlsx_es_path, "es")
write_user_xlsx_export(result, xlsx_en_path, "en")
saveRDS(result, rds_path)
zip_path <- build_reproducibility_export(result, zip_path)
zip_entries <- gsub("\\\\", "/", utils::unzip(zip_path, list = TRUE)$Name)
xlsx_es_entries <- gsub("\\\\", "/", utils::unzip(xlsx_es_path, list = TRUE)$Name)
xlsx_en_entries <- gsub("\\\\", "/", utils::unzip(xlsx_en_path, list = TRUE)$Name)
xlsx_es_xml <- xml2::read_xml(unz(xlsx_es_path, "xl/workbook.xml"))
xlsx_en_xml <- xml2::read_xml(unz(xlsx_en_path, "xl/workbook.xml"))
xlsx_es_sheets <- xml2::xml_attr(xml2::xml_find_all(xlsx_es_xml, ".//*[local-name()='sheet']"), "name")
xlsx_en_sheets <- xml2::xml_attr(xml2::xml_find_all(xlsx_en_xml, ".//*[local-name()='sheet']"), "name")
registry_choices <- registry_choice_vector(registry)
blank_filtered_registry <- filter_system_registry(registry, canton = "", parish = "")
map_registry <- registry_map_rows(blank_filtered_registry)
first_canton <- registry_filter_choices(registry$canton_display)[[1]]
first_canton_registry <- filter_system_registry(registry, canton = first_canton, parish = "")
missing_admin <- !nzchar(registry$canton_display) | !nzchar(registry$parish_display)
crosswalk <- load_administrative_label_crosswalk()
source_admin <- rbind(
  data.frame(level = "canton", source_value = registry_text_or_blank(registry$canton), stringsAsFactors = FALSE),
  data.frame(level = "parish", source_value = registry_text_or_blank(registry$parish), stringsAsFactors = FALSE)
)
source_admin <- unique(source_admin[nzchar(source_admin$source_value), , drop = FALSE])
source_admin_keys <- paste(source_admin$level, source_admin$source_value, sep = "\r")
crosswalk_keys <- paste(crosswalk$level, crosswalk$source_value, sep = "\r")
canton_display_values <- unique(registry_text_or_blank(registry$canton_display))
canton_display_values <- canton_display_values[nzchar(canton_display_values)]
parish_display_values <- unique(registry_text_or_blank(registry$parish_display))
parish_display_values <- parish_display_values[nzchar(parish_display_values)]
per_canton_parish_unique <- vapply(
  registry_filter_choices(registry$canton_display),
  function(cn) {
    z <- filter_system_registry(registry, canton = cn, parish = "__ALL__")
    vals <- unique(registry_text_or_blank(z$parish_display))
    vals <- vals[nzchar(vals)]
    !anyDuplicated(registry_label_key(vals))
  },
  logical(1)
)
reference_search_query <- selected_row$system_name[[1]]
reference_search <- search_system_registry(registry, reference_search_query, max_results = 25L)
accent_search_fixture <- data.frame(
  public_id = "AZU-TEST-001",
  system_name = "Sistema de Riego Río Azul",
  stringsAsFactors = FALSE
)
accent_search <- search_system_registry(accent_search_fixture, "RIO AZUL", max_results = 25L)
first_canton_bounds <- registry_map_bounds(filter_system_registry(registry, canton = first_canton, parish = "__ALL__"))
public_selector_summary <- as.data.frame(
  read_csv_utf8(file.path("data", "audit", "territorial_selector_public_summary_v1.csv")),
  stringsAsFactors = FALSE
)
if (restricted_mode) {
  ambiguity_audit <- as.data.frame(
    read_csv_utf8(file.path("data", "audit", "territorial_name_ambiguity_audit_v1.csv")),
    stringsAsFactors = FALSE
  )
  ambiguity_evidence_ok <- nrow(ambiguity_audit) >= 1L &&
    all(nzchar(ambiguity_audit$interpretation_note))
  ambiguity_observed <- paste(ambiguity_audit$ambiguity_type, collapse = " | ")
} else {
  ambiguity_evidence_ok <- all(c("metric", "value", "public_interpretation") %in% names(public_selector_summary)) &&
    all(nzchar(public_selector_summary$public_interpretation)) &&
    any(public_selector_summary$metric == "duplicate_normalised_system_name_records") &&
    any(public_selector_summary$metric == "combined_parish_source_labels")
  ambiguity_observed <- paste(public_selector_summary$metric, public_selector_summary$value, sep = "=", collapse = " | ")
}
app_text <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
daily_plan <- build_daily_action_plan_table(result)
expected_daily_dates <- seq(
  as.Date(inputs$current_turn_date),
  as.Date(inputs$next_turn_date) - 1,
  by = "day"
)
observed_daily_dates <- as.Date(daily_plan$date)
daily_dates_match <-
  length(observed_daily_dates) == length(expected_daily_dates) &&
  !anyNA(observed_daily_dates) &&
  all(as.character(observed_daily_dates) == as.character(expected_daily_dates))
daily_dates_unique <- !anyDuplicated(as.character(observed_daily_dates))
daily_date_steps <- if (length(observed_daily_dates) <= 1L) integer(0) else as.integer(diff(observed_daily_dates))
daily_dates_consecutive <- length(daily_date_steps) == 0L || all(daily_date_steps == 1L)
html_text <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
initial_presets <- load_initial_state_presets()
starter_text <- paste(readLines("INICIAR_AQUI.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
gate_pos <- regexpr('source("00_syntax_gate.R"', starter_text, fixed = TRUE)[[1]]
install_pos <- regexpr('source("00_install_packages.R"', starter_text, fixed = TRUE)[[1]]

half_lat <- 0.000225
half_lng <- half_lat / cos(productive_latitude * pi / 180)
synthetic_polygon_feature <- list(
  type = "Feature",
  properties = list(),
  geometry = list(
    type = "Polygon",
    coordinates = list(list(
      c(productive_longitude - half_lng, productive_latitude - half_lat),
      c(productive_longitude + half_lng, productive_latitude - half_lat),
      c(productive_longitude + half_lng, productive_latitude + half_lat),
      c(productive_longitude - half_lng, productive_latitude + half_lat),
      c(productive_longitude - half_lng, productive_latitude - half_lat)
    ))
  )
)
polygon_metrics <- polygon_metrics_from_feature(synthetic_polygon_feature)
polygon_inputs <- inputs
polygon_inputs$productive_latitude <- polygon_metrics$centroid_latitude
polygon_inputs$productive_longitude <- polygon_metrics$centroid_longitude
polygon_inputs$productive_location_source <- "polygon_centroid"
polygon_inputs$productive_polygon_geojson <- jsonlite::toJSON(synthetic_polygon_feature, auto_unbox = TRUE)
polygon_inputs$polygon_area_ha <- polygon_metrics$area_ha
polygon_inputs$polygon_centroid_latitude <- polygon_metrics$centroid_latitude
polygon_inputs$polygon_centroid_longitude <- polygon_metrics$centroid_longitude
polygon_inputs$area_source <- "polygon"
polygon_inputs$area_ha <- polygon_metrics$area_ha
polygon_result <- compute_riegoazuay_plan(polygon_inputs, registry, app_config)
polygon_ratio_expected <- polygon_metrics$area_ha / inputs$area_ha
polygon_ratio_observed <- polygon_result$central_summary$required_volume_m3[[1]] / result$central_summary$required_volume_m3[[1]]
scenario_csv_export <- as.data.frame(read_csv_utf8(csv_path), stringsAsFactors = FALSE)
point_warning_codes <- result$warnings_table$warning_code
polygon_warning_codes <- polygon_result$warnings_table$warning_code
summary_es <- build_user_summary_table(result, "es")
summary_polygon_es <- build_user_summary_table(polygon_result, "es")
user_sources_es <- build_user_source_table(result, "es")
user_sources_en <- build_user_source_table(result, "en")
expected_source_items_es <- c("Ubicación del cultivo", "Área del cultivo", "Meteorología", "Suelo", "Caudal", "Calendario de turnos")
expected_source_items_en <- c("Crop location", "Crop area", "Weather", "Soil", "Flow", "Turn schedule")
user_sources_es_text <- paste(unlist(user_sources_es, use.names = FALSE), collapse = " | ")
user_sources_en_text <- paste(unlist(user_sources_en, use.names = FALSE), collapse = " | ")
expected_soil_source_es <- switch(
  soil_mode,
  archived_santul = "Referencia pública SoilGrids 2.0 guardada para el punto de demostración de Santul",
  fao_reference = "Suelo general de referencia FAO",
  local_uniform = if (isTRUE(inputs$local_soil_verified)) "Perfil local de suelo verificado o documentado" else "Perfil local de suelo declarado por el usuario",
  live_soilgrids = "Referencia pública SoilGrids 2.0 consultada para la ubicación de la parcela",
  "Fuente del suelo registrada en la exportación técnica"
)
expected_soil_source_en <- switch(
  soil_mode,
  archived_santul = "Saved SoilGrids 2.0 public reference for the Santul demonstration point",
  fao_reference = "General FAO reference soil",
  local_uniform = if (isTRUE(inputs$local_soil_verified)) "Verified/documented local soil profile" else "User-declared local soil profile",
  live_soilgrids = "SoilGrids 2.0 public reference queried for the parcel location",
  "Soil source recorded in the technical export"
)
summary_es_text <- paste(summary_es$value, collapse = " | ")
summary_en_text <- paste(build_user_summary_table(result, "en")$value, collapse = " | ")
evidence_levels_runtime <- load_evidence_levels()

central <- result$central_summary
tests <- dplyr::bind_rows(
  new_test_row("INT01", "Registry mode is resolved", registry_mode %in% c("restricted", "demo"), registry_mode, "restricted or demo"),
  new_test_row("INT02", "Active registry has the expected record count", nrow(registry) == expected_registry_n, nrow(registry), expected_registry_n),
  new_test_row("INT03", "Selected application record is unique", nrow(selected_row) == 1L, nrow(selected_row), 1L),
  new_test_row("INT04", "Three weather scenarios are evaluated", length(unique(result$scenario_summary$weather_scenario)) == 3L, paste(unique(result$scenario_summary$weather_scenario), collapse = ","), 3L),
  new_test_row("INT05", "Expected soil scenarios are evaluated", length(unique(result$scenario_summary$soil_scenario)) == expected_soil_n, paste(unique(result$scenario_summary$soil_scenario), collapse = ","), expected_soil_n),
  new_test_row("INT06", "Combined uncertainty grid has the expected size", nrow(result$scenario_summary) == expected_combined_n, nrow(result$scenario_summary), expected_combined_n),
  new_test_row("INT07", "Central soil-weather scenario is unique", nrow(central) == 1L, nrow(central), 1L),
  new_test_row("INT08", "The controlled alpha scenario remains E2", identical(result$readiness$level, "E2"), result$readiness$level, "E2"),
  new_test_row("INT09", "Maximum mass-balance residual is negligible", max(abs(result$daily_all$balance_residual_mm), na.rm = TRUE) <= 1e-8, max(abs(result$daily_all$balance_residual_mm), na.rm = TRUE), "<=1e-8"),
  new_test_row("INT10", "All required volumes are nonnegative", all(result$scenario_summary$required_volume_m3 >= 0), min(result$scenario_summary$required_volume_m3), ">=0"),
  new_test_row("INT11", "All unmet volumes are nonnegative", all(result$scenario_summary$unmet_volume_m3 >= 0), min(result$scenario_summary$unmet_volume_m3), ">=0"),
  new_test_row("INT12", "Turn capacity is 43.2 m3 in central scenario", nrow(result$central_events) == 1L && abs(result$central_events$turn_capacity_m3[[1]] - 43.2) <= 1e-9, if (nrow(result$central_events)) result$central_events$turn_capacity_m3[[1]] else NA_real_, 43.2),
  new_test_row("INT13", "Provenance table covers all decision-critical input groups", nrow(result$provenance) >= 14L, nrow(result$provenance), ">=14"),
  new_test_row("INT14", "Provisional warning is retained", any(grepl("provisional", result$warnings, ignore.case = TRUE)), paste(result$warnings, collapse = " | "), "contains provisional"),
  new_test_row("INT15", "Bilingual HTML report is generated", file.exists(html_path) && grepl("Español", paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), fixed = TRUE) && grepl("English", paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), fixed = TRUE), file.exists(html_path), TRUE),
  new_test_row("INT16", "Machine-readable result CSV is generated", file.exists(csv_path), file.exists(csv_path), TRUE),
  new_test_row("INT17", "Scenario RDS is generated", file.exists(rds_path), file.exists(rds_path), TRUE),
  new_test_row("INT18", "Reproducibility ZIP is generated", file.exists(zip_path), file.exists(zip_path), TRUE),
  new_test_row("INT19", "Reproducibility ZIP contains selected system only", any(grepl("selected_system_only.csv$", zip_entries)) && !any(grepl("systems_registry_159_restricted.csv$", zip_entries)), paste(zip_entries, collapse = " | "), "selected system present; restricted registry absent"),
  new_test_row("INT20", "Reproducibility ZIP contains SHA-256 manifest", any(grepl("MANIFEST_SHA256.csv$", zip_entries)), paste(zip_entries, collapse = " | "), "manifest present"),
  new_test_row("INT21", "Registry display labels are complete for system selection", all(!is.na(registry$display_label) & nzchar(registry$display_label)), sum(is.na(registry$display_label) | !nzchar(registry$display_label)), 0L),
  new_test_row("INT22", "System choices have complete values and names", all(!is.na(registry_choices) & nzchar(registry_choices)) && all(!is.na(names(registry_choices)) & nzchar(names(registry_choices))), sum(is.na(registry_choices) | !nzchar(registry_choices)) + sum(is.na(names(registry_choices)) | !nzchar(names(registry_choices))), 0L),
  new_test_row("INT23", "Blank initial filters retain the full registry", nrow(blank_filtered_registry) == nrow(registry), nrow(blank_filtered_registry), nrow(registry)),
  new_test_row("INT24", "Canton filtering excludes missing-value phantom rows", nrow(first_canton_registry) > 0L && all(registry_text_or_blank(first_canton_registry$canton_display) == first_canton) && all(is.finite(first_canton_registry$latitude)) && all(is.finite(first_canton_registry$longitude)), nrow(first_canton_registry), "positive rows; selected harmonised canton only; finite coordinates"),
  new_test_row("INT25", "Missing canton/parish values do not remove selectable systems", if (restricted_mode) sum(missing_admin) == 5L && all(nzchar(registry$display_label[missing_admin])) else all(nzchar(registry$display_label)), if (restricted_mode) sum(missing_admin) else 0L, if (restricted_mode) 5L else 0L),
  new_test_row("INT26", "Startup map rows contain only finite coordinates and no phantom rows", nrow(map_registry) == nrow(registry) && all(is.finite(map_registry$latitude)) && all(is.finite(map_registry$longitude)), nrow(map_registry), nrow(registry)),
  new_test_row("INT27", "Administrative crosswalk covers every nonblank active source label", all(source_admin_keys %in% crosswalk_keys), sum(!source_admin_keys %in% crosswalk_keys), 0L),
  new_test_row("INT28", "Harmonised canton labels contain no normalised duplicates", !anyDuplicated(registry_label_key(canton_display_values)), paste(canton_display_values, collapse = " | "), "unique normalised keys"),
  new_test_row("INT29", "Harmonised parish labels contain no normalised duplicates", !anyDuplicated(registry_label_key(parish_display_values)), length(parish_display_values), "unique normalised keys"),
  new_test_row("INT30", "Parish choices are unique within every harmonised canton", all(per_canton_parish_unique), paste(names(per_canton_parish_unique)[!per_canton_parish_unique], collapse = " | "), "no duplicated normalised parish label within canton"),
  new_test_row("INT31", "Restricted registry yields 13 harmonised canton choices", if (restricted_mode) length(registry_filter_choices(registry$canton_display)) == 13L else length(registry_filter_choices(registry$canton_display)) >= 1L, length(registry_filter_choices(registry$canton_display)), if (restricted_mode) 13L else ">=1"),
  new_test_row("INT32", "Map validation uses Shiny namespace and jsonlite is not attached", grepl("shiny::validate(shiny::need", app_text, fixed = TRUE) && !grepl("library(jsonlite)", app_text, fixed = TRUE), "namespace audit", "shiny::validate present; library(jsonlite) absent"),
  new_test_row(
    "INT33",
    "Daily action calendar covers every day through the day before the next turn",
    daily_dates_match,
    paste0("n=", length(observed_daily_dates), "; dates=", paste(as.character(observed_daily_dates), collapse = ",")),
    paste0("n=", length(expected_daily_dates), "; dates=", paste(as.character(expected_daily_dates), collapse = ","))
  ),
  new_test_row("INT34", "Daily action calendar contains exactly one current-turn day", sum(daily_plan$turn_day) == 1L && daily_plan$date[daily_plan$turn_day][[1]] == as.Date(inputs$current_turn_date), sum(daily_plan$turn_day), 1L),
  new_test_row("INT35", "No water is assigned outside the declared turn", all(daily_plan$planned_volume_m3[!daily_plan$turn_day] == 0), max(daily_plan$planned_volume_m3[!daily_plan$turn_day]), 0),
  new_test_row("INT36", "Turn-day volume equals the central deliverable volume", abs(daily_plan$planned_volume_m3[daily_plan$turn_day][[1]] - central$deliverable_volume_m3[[1]]) <= 1e-9, daily_plan$planned_volume_m3[daily_plan$turn_day][[1]], central$deliverable_volume_m3[[1]]),
  new_test_row("INT37", "Daily action calendar contains complete Spanish and English guidance", all(nzchar(daily_plan$action_es)) && all(nzchar(daily_plan$action_en)) && all(nzchar(daily_plan$note_es)) && all(nzchar(daily_plan$note_en)), "bilingual daily actions", "all nonblank"),
  new_test_row("INT38", "Bilingual HTML report contains both daily-plan headings", grepl("Calendario diario de acciones", html_text, fixed = TRUE) && grepl("Daily action calendar", html_text, fixed = TRUE), "HTML heading audit", "Spanish and English daily plan headings"),
  new_test_row("INT39", "Reproducibility ZIP contains the bilingual daily action plan", any(grepl("daily_action_plan_ES_EN.csv$", zip_entries)), paste(zip_entries[grepl("daily_action_plan", zip_entries)], collapse = " | "), "daily_action_plan_ES_EN.csv"),
  new_test_row("INT40", "Simple initial-water presets are complete and remain unverified declarations", nrow(initial_presets) == 4L && all(initial_presets$provenance_class == "USER_DECLARED") && !any(as.logical(initial_presets$verified)), nrow(initial_presets), "4 USER_DECLARED unverified presets"),
  new_test_row("INT41", "Application includes writable name/ID search and persistent navigation", grepl('textInput(\n            "system_search_query"', app_text, fixed = TRUE) && grepl("search_system_registry", app_text, fixed = TRUE) && grepl('actionButton("nav_back"', app_text, fixed = TRUE) && grepl('actionButton("nav_next"', app_text, fixed = TRUE), "UI source audit", "text search + Back/Next"),
  new_test_row("INT42", "Simple results prioritise the daily plan while technical diagnostics remain optional", grepl('uiOutput("daily_plan_ui")', app_text, fixed = TRUE) && grepl('"input.technical_view == true"', app_text, fixed = TRUE) && grepl('plotOutput("scenario_range_plot"', app_text, fixed = TRUE), "UI source audit", "daily plan visible; uncertainty detail technical"),
  new_test_row("INT43", "Bilingual navigation dictionary uses identical keys and the nonreserved next_step identifier", identical(names(app_dictionary$es), names(app_dictionary$en)) && "next_step" %in% names(app_dictionary$es) && !("next" %in% names(app_dictionary$es)) && grepl('tr_app("next_step", l)', app_text, fixed = TRUE), "dictionary/navigation audit", "identical keys; next_step present; next absent"),
  new_test_row("INT44", "Syntax gate is invoked before installation and application tests", file.exists("00_syntax_gate.R") && gate_pos > 0L && install_pos > gate_pos, paste(gate_pos, install_pos, sep = " / "), "syntax gate before installer"),
  new_test_row("INT45", "Daily action calendar contains no duplicated dates", daily_dates_unique, paste(as.character(observed_daily_dates[duplicated(as.character(observed_daily_dates))]), collapse = " | "), "no duplicated date"),
  new_test_row("INT46", "Daily action calendar advances in one-day increments", daily_dates_consecutive, paste(daily_date_steps, collapse = " | "), "all increments equal 1 day"),
  new_test_row("INT47", "Direct system search finds the active reference system by name", nrow(reference_search) >= 1L && selected_id %in% reference_search$public_id, paste(reference_search$public_id, collapse = " | "), paste("contains", selected_id)),
  new_test_row("INT48", "Direct name/ID search is accent- and case-insensitive within system names", nrow(accent_search) == 1L && identical(accent_search$public_id[[1]], "AZU-TEST-001"), paste(accent_search$public_id, accent_search$system_name, sep = " | ", collapse = " || "), "AZU-TEST-001 | Sistema de Riego Río Azul"),
  new_test_row("INT49", "Canton map bounds remain finite and parish-independent", length(first_canton_bounds) == 4L && all(is.finite(first_canton_bounds)) && first_canton_bounds[["lng2"]] > first_canton_bounds[["lng1"]] && first_canton_bounds[["lat2"]] > first_canton_bounds[["lat1"]], paste(first_canton_bounds, collapse = " | "), "finite ordered canton bounds"),
  new_test_row("INT50", "Simple territory workflow exposes point and guided polygon controls", grepl('actionButton("activate_point_picker"', app_text, fixed = TRUE) && grepl('actionButton("activate_polygon_draw"', app_text, fixed = TRUE) && grepl('uiOutput("polygon_area_choice_ui")', app_text, fixed = TRUE), "UI source audit", "point picker, polygon drawing, and explicit area choice"),
  new_test_row("INT51", "Map visually distinguishes intake, productive point, and connection distance", grepl("map_legend_title", app_text, fixed = TRUE) && grepl("addPolylines", app_text, fixed = TRUE) && grepl("intake_productive_distance", app_text, fixed = TRUE), "map source audit", "legend and intake-to-point line"),
  new_test_row("INT52", "Spanish user Excel export is a valid OOXML workbook with four user sheets", file.exists(xlsx_es_path) && all(c("xl/workbook.xml", "[Content_Types].xml") %in% xlsx_es_entries) && identical(xlsx_es_sheets, c("Resumen parcela", "Plan diario", "Advertencias", "Fuentes")), paste(xlsx_es_sheets, collapse = " | "), "Resumen parcela | Plan diario | Advertencias | Fuentes"),
  new_test_row("INT53", "English user Excel export is a valid OOXML workbook with four user sheets", file.exists(xlsx_en_path) && all(c("xl/workbook.xml", "[Content_Types].xml") %in% xlsx_en_entries) && identical(xlsx_en_sheets, c("Parcel summary", "Daily plan", "Warnings", "Sources")), paste(xlsx_en_sheets, collapse = " | "), "Parcel summary | Daily plan | Warnings | Sources"),
  new_test_row("INT54", "Reproducibility ZIP contains Spanish and English user Excel plans", any(grepl("RiegoAzuay_user_plan_ES.xlsx$", zip_entries)) && any(grepl("RiegoAzuay_user_plan_EN.xlsx$", zip_entries)), paste(zip_entries[grepl("user_plan_", zip_entries)], collapse = " | "), "ES and EN user workbooks"),
  new_test_row("INT55", "Simple downloads prioritise Excel and printable HTML while raw files remain technical", grepl('downloadButton("download_user_excel"', app_text, fixed = TRUE) && grepl('downloadButton("download_report"', app_text, fixed = TRUE) && grepl("technical-downloads", app_text, fixed = TRUE), "download UI audit", "Excel/HTML primary; raw exports technical"),
  new_test_row("INT56", "Territorial ambiguities are documented at the permitted disclosure level", ambiguity_evidence_ok, ambiguity_observed, if (restricted_mode) "controlled record-level audit" else "non-identifying aggregate summary"),
  new_test_row("INT57", "Simple evidence view prioritises actions and core source types", grepl("priority_actions_title", app_text, fixed = TRUE) && grepl("source-summary-grid", app_text, fixed = TRUE) && !grepl("counts <- table(factor", app_text, fixed = TRUE), "evidence UI audit", "action list and source summary; no raw counts"),
  new_test_row(
    "INT58",
    "Reference name search returns only true name/ID matches",
    selected_id %in% reference_search$public_id &&
      all(vapply(
        strsplit(normalise_system_search_text(reference_search_query), " ", fixed = TRUE)[[1]],
        function(token) all(grepl(
          token,
          normalise_system_search_text(paste(reference_search$public_id, reference_search$system_name)),
          fixed = TRUE
        )),
        logical(1)
      )),
    paste(reference_search$public_id, collapse = " | "),
    paste("contains", selected_id, "and no unrelated records")
  ),
  new_test_row("INT59", "A point-based run keeps declared crop area without inventing a footprint", identical(result$inputs$area_source, "declared") && abs(result$inputs$area_ha - result$inputs$declared_area_ha) <= 1e-12 && is.na(result$inputs$polygon_area_ha), paste(result$inputs$area_source, result$inputs$area_ha, result$inputs$polygon_area_ha, sep = " | "), "declared area; no polygon area"),
  new_test_row("INT60", "Point mode produces an explicit no-footprint warning", "POINT_NO_FOOTPRINT" %in% point_warning_codes, paste(point_warning_codes, collapse = " | "), "POINT_NO_FOOTPRINT"),
  new_test_row("INT61", "Synthetic crop polygon yields a positive area and finite centroid", !is.null(polygon_metrics) && polygon_metrics$area_ha > 0 && all(is.finite(c(polygon_metrics$centroid_latitude, polygon_metrics$centroid_longitude))), paste(polygon_metrics$area_ha, polygon_metrics$centroid_latitude, polygon_metrics$centroid_longitude, sep = " | "), "positive area and finite centroid"),
  new_test_row("INT62", "Polygon centroid remains at the representative crop location", abs(polygon_metrics$centroid_latitude - productive_latitude) <= 1e-8 && abs(polygon_metrics$centroid_longitude - productive_longitude) <= 1e-8, paste(polygon_metrics$centroid_latitude, polygon_metrics$centroid_longitude, sep = " | "), paste(productive_latitude, productive_longitude, sep = " | ")),
  new_test_row("INT63", "Explicit polygon choice controls the active crop area", identical(polygon_result$inputs$area_source, "polygon") && abs(polygon_result$inputs$area_ha - polygon_metrics$area_ha) <= 1e-10, paste(polygon_result$inputs$area_source, polygon_result$inputs$area_ha, sep = " | "), paste("polygon", polygon_metrics$area_ha, sep = " | ")),
  new_test_row("INT64", "Required volume scales with explicitly selected polygon area", is.finite(polygon_ratio_observed) && abs(polygon_ratio_observed - polygon_ratio_expected) <= 1e-8, polygon_ratio_observed, polygon_ratio_expected),
  new_test_row("INT65", "Polygon run records the crop-specific area warning", "POLYGON_AREA_USED" %in% polygon_warning_codes && "CROP_AREA_SCOPE" %in% polygon_warning_codes, paste(polygon_warning_codes, collapse = " | "), "POLYGON_AREA_USED and CROP_AREA_SCOPE"),
  new_test_row("INT66", "Scenario CSV preserves decision unit, area source, and spatial provenance", all(c("decision_unit", "effective_crop_area_ha", "area_source", "declared_crop_area_ha", "polygon_area_ha", "productive_location_source") %in% names(scenario_csv_export)) && all(scenario_csv_export$decision_unit == "single_crop_homogeneous_zone") && all(scenario_csv_export$area_source == "declared"), paste(names(scenario_csv_export), collapse = " | "), "crop-specific area and location metadata"),
  new_test_row("INT67", "User summary states one crop-specific parcel and the active area source", any(grepl("zona homogénea de un solo cultivo", summary_es$value, fixed = TRUE)) && any(grepl("Área del cultivo declarada", summary_es$value, fixed = TRUE)), paste(summary_es$value, collapse = " | "), "crop-specific decision unit and declared area source"),
  new_test_row("INT68", "Polygon user summary identifies polygon-derived area", any(grepl("polígono", summary_polygon_es$value, ignore.case = TRUE)) && any(summary_polygon_es$field == "Área del polígono (ha)"), paste(summary_polygon_es$value, collapse = " | "), "polygon-derived area and polygon field"),
  new_test_row("INT69", "Formatted Excel workbooks include explicit column-width metadata", any(grepl("xl/worksheets/sheet1.xml", xlsx_es_entries, fixed = TRUE)) && grepl("<cols", paste(readLines(unz(xlsx_es_path, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = ""), fixed = TRUE), "OOXML sheet1 column metadata", "<cols present"),
  new_test_row("INT70", "UI explicitly states that points do not distribute area around themselves", grepl("point_area_note", app_text, fixed = TRUE) && grepl("no genera un círculo", paste(unname(app_dictionary$es), collapse = " | "), fixed = TRUE), "point semantics audit", "no implicit footprint or buffer"),
  new_test_row("INT71", "UI requires polygon to represent one selected crop and invalidates it after crop change", grepl("polygon_selected_crop", app_text, fixed = TRUE) && grepl("polygon_crop_profile_id", app_text, fixed = TRUE) && grepl("polygon_crop_change_reset", app_text, fixed = TRUE), "polygon-crop consistency audit", "crop-specific polygon and change reset"),
  new_test_row("INT72", "Polygon workflow requires explicit declared-versus-polygon area choice", grepl('"area_source_choice"', app_text, fixed = TRUE) && grepl('radioButtons(', app_text, fixed = TRUE) && grepl("area_choice_required", app_text, fixed = TRUE) && grepl("effective_area_ha", app_text, fixed = TRUE), "area-choice audit", "explicit area choice drives engine input"),
  new_test_row("INT73", "Selected system resolves to a complete canton and parish target", nzchar(selected_admin_canton) && nzchar(selected_admin_parish), paste(selected_admin_canton, selected_admin_parish, sep = " / "), "nonblank canton / parish"),
  new_test_row("INT74", "Selected system parish belongs to the rebuilt canton-specific choices", selected_admin_parish %in% selected_admin_parish_choices, paste(selected_admin_parish_choices, collapse = " | "), paste("contains", selected_admin_parish)),
  new_test_row("INT75", "Runtime source uses staged client-confirmed administrative synchronisation", grepl("state$pending_canton <- desired_canton", app_text, fixed = TRUE) && grepl("if (!is.null(pending_canton) && !identical(active_canton, pending_canton)) return()", app_text, fixed = TRUE) && grepl("identical(active_canton, pending_canton) && identical(active_parish, pending_parish)", app_text, fixed = TRUE), "staged sync source audit", "canton acknowledged before parish and pending cleared after both"),
  new_test_row("INT76", "Evidence step is explicitly labelled as evidence and sources", identical(tr_app("step_evidence", "es"), "3. Revise evidencia y fuentes") && identical(tr_app("step_evidence", "en"), "3. Review evidence and sources"), paste(tr_app("step_evidence", "es"), tr_app("step_evidence", "en"), sep = " | "), "explicit evidence/source step labels"),
  new_test_row("INT77", "Scenario explains that turn frequency is not fixed weekly", grepl("no supone un riego semanal fijo", tr_app("turn_horizon_note", "es"), fixed = TRUE) && grepl("does not assume a fixed weekly irrigation schedule", tr_app("turn_horizon_note", "en"), fixed = TRUE), paste(tr_app("turn_horizon_note", "es"), tr_app("turn_horizon_note", "en"), sep = " | "), "arbitrary interval; one irrigation opportunity per horizon"),
  new_test_row("INT78", "E2 and E3 readiness labels are turn-level rather than weekly", all(!grepl("semanal|weekly", c(evidence_levels_runtime$label_es[evidence_levels_runtime$level %in% c("E2", "E3")], evidence_levels_runtime$label_en[evidence_levels_runtime$level %in% c("E2", "E3")]), ignore.case = TRUE)) && any(grepl("siguiente turno", evidence_levels_runtime$label_es[evidence_levels_runtime$level == "E2"], fixed = TRUE)) && any(grepl("turn-level", evidence_levels_runtime$label_en[evidence_levels_runtime$level == "E2"], fixed = TRUE)), paste(evidence_levels_runtime$label_es[evidence_levels_runtime$level %in% c("E2", "E3")], evidence_levels_runtime$label_en[evidence_levels_runtime$level %in% c("E2", "E3")], collapse = " | "), "turn-level terminology"),
  new_test_row("INT79", "User Excel source table omits raw Boolean verification while technical provenance retains it", identical(names(user_sources_es), c("Elemento", "Valor activo", "Tipo de fuente")) && identical(names(user_sources_en), c("Item", "Active value", "Source type")) && "verified" %in% names(result$provenance), paste(names(user_sources_es), names(user_sources_en), names(result$provenance), collapse = " | "), "three user columns; verified retained technically"),
  new_test_row("INT80", "Spanish user source table contains exactly six complete named rows", nrow(user_sources_es) == 6L && identical(user_sources_es$Elemento, expected_source_items_es) && all(nzchar(trimws(user_sources_es$`Valor activo`))), paste(user_sources_es$Elemento, collapse = " | "), paste(expected_source_items_es, collapse = " | ")),
  new_test_row("INT81", "English user source table contains exactly six complete named rows", nrow(user_sources_en) == 6L && identical(user_sources_en$Item, expected_source_items_en) && all(nzchar(trimws(user_sources_en$`Active value`))), paste(user_sources_en$Item, collapse = " | "), paste(expected_source_items_en, collapse = " | ")),
  new_test_row("INT82", "Spanish user source values are localised and match the active soil mode", !grepl("scope=|verified=|single_crop_homogeneous_zone|Synthetic demonstration weather data|Archived SoilGrids|; declared", user_sources_es_text, ignore.case = FALSE) && grepl("Datos meteorológicos sintéticos de demostración", user_sources_es_text, fixed = TRUE) && grepl(expected_soil_source_es, user_sources_es_text, fixed = TRUE), user_sources_es_text, paste0("localised Spanish source descriptions; soil=", expected_soil_source_es)),
  new_test_row("INT83", "English user source values are localised and match the active soil mode", !grepl("scope=|verified=|single_crop_homogeneous_zone", user_sources_en_text, ignore.case = FALSE) && grepl("Synthetic demonstration weather data", user_sources_en_text, fixed = TRUE) && grepl(expected_soil_source_en, user_sources_en_text, fixed = TRUE), user_sources_en_text, paste0("localised English source descriptions; soil=", expected_soil_source_en)),
  new_test_row("INT84", "User summaries localise the productive-location source", grepl("Coordenada ingresada por el usuario", summary_es_text, fixed = TRUE) && grepl("User-entered coordinate", summary_en_text, fixed = TRUE) && !grepl("map_click|manual|polygon_centroid|intake_approximation", paste(summary_es_text, summary_en_text), ignore.case = FALSE), paste(summary_es_text, summary_en_text, sep = " | "), "human-readable location source in ES and EN")
)

write_csv_utf8(tests, file.path("outputs", "tests", "integration_smoke_tests.csv"))
write_csv_utf8(result$envelope, file.path(smoke_dir, "smoke_envelope.csv"))
write_csv_utf8(result$provenance, file.path(smoke_dir, "smoke_provenance.csv"))
write_csv_utf8(result$diagnostics, file.path(smoke_dir, "smoke_diagnostics.csv"))
print_test_summary(tests, "RESULTADO GENERAL: LA PRUEBA DE INTEGRACIÓN REGISTRÓ FALLOS.")
cat("RESULTADO GENERAL: PRUEBA DE INTEGRACIÓN SUPERADA.\n")
cat("Smoke evidence written to: ", normalizePath(smoke_dir, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat('Next step: source("04_run_app.R")\n')
