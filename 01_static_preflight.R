source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "app_data.R"), encoding = "UTF-8")
source(file.path("R", "app_i18n.R"), encoding = "UTF-8")

cat("\n=== RIEGOAZUAY DSS v1.0.0: STATIC PREFLIGHT ===\n")

tests <- list()
add_test <- function(id, description, pass, observed, expected) {
  tests[[length(tests) + 1L]] <<- new_test_row(id, description, isTRUE(pass), observed, expected)
}

required_root <- c(
  "app.R", "VERSION", "RELEASE_TAG", "RiegoAzuay-DSS.Rproj", ".gitignore",
  "00_syntax_gate.R", "00_install_packages.R", "01_static_preflight.R", "02_run_scientific_tests.R",
  "03_run_integration_smoke_tests.R", "04_run_app.R", "05_freeze_environment.R",
  "06_prepare_public_release.R", "07_run_live_source_audit.R", "08_run_packaging_manifest_gate.R",
  "09_run_release_candidate_gate.R", "PREPARAR_CANDIDATA_RC6.R", "VERIFY_PUBLIC_PACKAGE.R",
  "AUDITAR_FUENTES_EN_VIVO.R", "AUDITAR_EMPAQUETADO_Y_MANIFIESTOS.R", "INICIAR_AQUI.R", "LEAME_PRIMERO.txt", "README.md",
  "CITATION.cff", "LICENSE"
)
required_r <- c(
  "utils.R", "app_i18n.R", "profile_library.R", "fao56_daily_balance.R",
  "layered_soil_balance.R", "irrigation_scheduler.R", "layered_irrigation_scheduler.R",
  "evidence_gate.R", "flow_measurement_assistant.R", "forecast_archive.R",
  "soilgrids_gateway.R", "soil_profile_integration.R", "app_data.R", "app_weather.R",
  "app_soil.R", "app_engine.R", "app_reporting.R", "app_ui_helpers.R", "scientific_tests.R"
)
required_config <- c(
  "administrative_label_crosswalk_v1.csv", "app_config.csv", "crop_profiles_daily_v2.csv", "evidence_levels_v1.csv",
  "flow_method_decision_tree.csv", "flow_method_register_v1.csv", "provenance_classes.csv",
  "sdg_information_layer.csv", "simple_initial_state_presets_v1.csv", "soil_integration_config.csv", "soil_reference_profiles_v1.csv",
  "soilgrids_layer_register.csv", "source_register_v2.csv", "runtime_packages_v1.csv"
)
restricted_registry_path_preflight <- file.path("data", "restricted", "systems_registry_159_restricted.csv")
restricted_present_preflight <- file.exists(restricted_registry_path_preflight)
required_audit_data <- c(
  "territorial_selector_summary.json",
  "territorial_selector_public_summary_v1.csv"
)
if (restricted_present_preflight) {
  required_audit_data <- c(
    required_audit_data,
    "territorial_selector_audit_final_v1.csv",
    "territorial_name_ambiguity_audit_v1.csv"
  )
}
required_data <- c(
  file.path("audit", required_audit_data),
  file.path("demo", c("systems_registry_demo.csv", "weather_relative_fixture.csv",
                      "archived_selected_forecast_20261025.csv",
                      "soil_layer_profiles_santul_archived.csv",
                      "root_zone_soil_summary_santul_archived.csv")),
  file.path("restricted", "README.md"),
  file.path("templates", "manual_weather_template.csv"),
  file.path("reference", c("fao56_example36_expected.csv", "fao56_example37_expected.csv")),
  file.path("fixtures", c("flow_float_demo.csv", "flow_measurement_context_demo.csv",
                          "flow_measurement_context_incomplete.csv", "flow_volumetric_demo.csv",
                          "weather_7day_ensemble_demo.csv", "weather_to_next_turn_ensemble_demo.csv"))
)

n <- 1L
for (f in required_root) {
  add_test(sprintf("FILE%03d", n), paste("Root file exists:", f), file.exists(f), file.exists(f), TRUE); n <- n + 1L
}
for (f in required_r) {
  p <- file.path("R", f)
  add_test(sprintf("FILE%03d", n), paste("R module exists:", f), file.exists(p), file.exists(p), TRUE); n <- n + 1L
}
for (f in required_config) {
  p <- file.path("config", f)
  add_test(sprintf("FILE%03d", n), paste("Configuration exists:", f), file.exists(p), file.exists(p), TRUE); n <- n + 1L
}
for (f in required_data) {
  p <- file.path("data", f)
  add_test(sprintf("FILE%03d", n), paste("Data/fixture exists:", f), file.exists(p), file.exists(p), TRUE); n <- n + 1L
}
if (!restricted_present_preflight) {
  add_test(
    "PUB001",
    "Public build omits the record-level selector audit",
    !file.exists(file.path("data", "audit", "territorial_selector_audit_final_v1.csv")),
    file.exists(file.path("data", "audit", "territorial_selector_audit_final_v1.csv")),
    FALSE
  )
  add_test(
    "PUB002",
    "Public build omits the record-level name-ambiguity audit",
    !file.exists(file.path("data", "audit", "territorial_name_ambiguity_audit_v1.csv")),
    file.exists(file.path("data", "audit", "territorial_name_ambiguity_audit_v1.csv")),
    FALSE
  )
}

version <- trimws(readLines("VERSION", warn = FALSE, encoding = "UTF-8")[[1]])
tag <- trimws(readLines("RELEASE_TAG", warn = FALSE, encoding = "UTF-8")[[1]])
add_test("VER001", "Version is the final public release", identical(version, "1.0.0"), version, "1.0.0")
add_test("VER002", "Release tag matches the final public release", identical(tag, "RiegoAzuay-DSS-v1.0.0"), tag, "RiegoAzuay-DSS-v1.0.0")
add_test("VER003", "Final version and tag contain no release-candidate suffix", !grepl("-rc", version, fixed = TRUE) && !grepl("-rc", tag, fixed = TRUE), paste(version, tag, sep = " | "), "no -rc suffix")

restricted_registry_path <- file.path("data", "restricted", "systems_registry_159_restricted.csv")
demo_registry_path <- file.path("data", "demo", "systems_registry_demo.csv")
restricted_present <- file.exists(restricted_registry_path)
preflight_config <- load_app_config()
configured_demo_id_preflight <- trimws(as.character(preflight_config$scientific_demo_system_id))
registry <- as.data.frame(read_csv_utf8(if (restricted_present) restricted_registry_path else demo_registry_path), stringsAsFactors = FALSE)
if (restricted_present) {
  expected_ids <- sprintf("AZU-SR-%03d", seq_len(159L))
  add_test("REG001", "Restricted selector contains exactly 159 systems", nrow(registry) == 159L, nrow(registry), 159L)
  add_test("REG002", "Public application IDs are contiguous AZU-SR-001 to AZU-SR-159", identical(registry$public_id, expected_ids), paste(head(registry$public_id, 1), tail(registry$public_id, 1), sep = " … "), "AZU-SR-001 … AZU-SR-159")
  add_test("REG003", "Selector system names are present", all(nzchar(trimws(registry$system_name))), sum(!nzchar(trimws(registry$system_name))), 0L)
  add_test("REG004", "Selector coordinates are finite", all(is.finite(as.numeric(registry$latitude))) && all(is.finite(as.numeric(registry$longitude))), sum(!is.finite(as.numeric(registry$latitude))) + sum(!is.finite(as.numeric(registry$longitude))), 0L)
  add_test("REG005", "Coordinates are explicitly registered water-intake locations", all(registry$coordinate_semantics == "registered_water_intake_location"), paste(unique(registry$coordinate_semantics), collapse = " | "), "registered_water_intake_location")
  add_test("REG006", "Institutional registry remains marked restricted", all(registry$source_status == "restricted_institutional_reference"), paste(unique(registry$source_status), collapse = " | "), "restricted_institutional_reference")
  add_test("REG007", "Configured scientific demonstration record is unique", sum(registry$public_id == configured_demo_id_preflight) == 1L, sum(registry$public_id == configured_demo_id_preflight), 1L)
} else {
  add_test("REG001", "Restricted registry is intentionally absent from the public build", !restricted_present, restricted_present, FALSE)
  add_test("REG002", "Synthetic public fixture is available", nrow(registry) >= 1L && all(grepl("synthetic", registry$source_status, fixed = TRUE)), paste(registry$public_id, collapse = ","), "synthetic fixture")
  add_test("REG003", "Synthetic fixture coordinates are finite", all(is.finite(as.numeric(registry$latitude))) && all(is.finite(as.numeric(registry$longitude))), nrow(registry), "all finite")
  add_test("REG004", "Synthetic public application IDs are nonblank and unique", all(nzchar(trimws(registry$public_id))) && !anyDuplicated(registry$public_id), paste(registry$public_id, collapse = " | "), "nonblank unique IDs")
  add_test("REG005", "Public fixture coordinates are explicitly synthetic demonstration points", all(registry$coordinate_semantics == "synthetic_demonstration_point"), paste(unique(registry$coordinate_semantics), collapse = " | "), "synthetic_demonstration_point")
  add_test("REG006", "Public fixture remains marked as synthetic rather than institutional", all(registry$source_status == "synthetic_public_demo"), paste(unique(registry$source_status), collapse = " | "), "synthetic_public_demo")
  add_test("REG007", "First synthetic demonstration record is unique", sum(registry$public_id == registry$public_id[[1]]) == 1L, sum(registry$public_id == registry$public_id[[1]]), 1L)
}

app_config_runtime <- load_app_config()
registry_runtime <- load_system_registry(app_config_runtime)
registry_choices_runtime <- registry_choice_vector(registry_runtime)
blank_filtered_runtime <- filter_system_registry(registry_runtime, canton = "", parish = "")
map_rows_runtime <- registry_map_rows(blank_filtered_runtime)
first_canton_runtime <- registry_filter_choices(registry_runtime$canton_display)[[1]]
first_canton_filtered_runtime <- filter_system_registry(registry_runtime, canton = first_canton_runtime, parish = "")
missing_admin_runtime <- !nzchar(registry_runtime$canton_display) | !nzchar(registry_runtime$parish_display)
crosswalk_runtime <- load_administrative_label_crosswalk()
source_admin_runtime <- rbind(
  data.frame(level = "canton", source_value = registry_text_or_blank(registry_runtime$canton), stringsAsFactors = FALSE),
  data.frame(level = "parish", source_value = registry_text_or_blank(registry_runtime$parish), stringsAsFactors = FALSE)
)
source_admin_runtime <- unique(source_admin_runtime[nzchar(source_admin_runtime$source_value), , drop = FALSE])
source_admin_keys_runtime <- paste(source_admin_runtime$level, source_admin_runtime$source_value, sep = "\r")
crosswalk_keys_runtime <- paste(crosswalk_runtime$level, crosswalk_runtime$source_value, sep = "\r")
canton_display_values_runtime <- unique(registry_text_or_blank(registry_runtime$canton_display))
canton_display_values_runtime <- canton_display_values_runtime[nzchar(canton_display_values_runtime)]
parish_display_values_runtime <- unique(registry_text_or_blank(registry_runtime$parish_display))
parish_display_values_runtime <- parish_display_values_runtime[nzchar(parish_display_values_runtime)]
per_canton_parish_unique_runtime <- vapply(
  registry_filter_choices(registry_runtime$canton_display),
  function(cn) {
    z <- filter_system_registry(registry_runtime, canton = cn, parish = "__ALL__")
    vals <- unique(registry_text_or_blank(z$parish_display))
    vals <- vals[nzchar(vals)]
    !anyDuplicated(registry_label_key(vals))
  },
  logical(1)
)
configured_demo_id_runtime <- trimws(as.character(app_config_runtime$scientific_demo_system_id))
reference_id_runtime <- if (restricted_present && configured_demo_id_runtime %in% registry_runtime$public_id) configured_demo_id_runtime else registry_runtime$public_id[[1]]
reference_admin_runtime <- selected_registry_row(registry_runtime, reference_id_runtime)
reference_query_runtime <- registry_text_or_blank(reference_admin_runtime$system_name[[1]])
reference_search_runtime <- search_system_registry(registry_runtime, reference_query_runtime, max_results = 25L)
reference_canton_runtime <- registry_text_or_blank(reference_admin_runtime$canton_display[[1]])
reference_parish_runtime <- registry_text_or_blank(reference_admin_runtime$parish_display[[1]])
reference_parish_choices_runtime <- registry_filter_choices(
  filter_system_registry(registry_runtime, canton = reference_canton_runtime, parish = "__ALL__")$parish_display
)
polygon_test_feature <- list(
  type = "Feature", properties = list(),
  geometry = list(type = "Polygon", coordinates = list(list(
    c(-78.6760, -2.7240), c(-78.6755, -2.7240), c(-78.6755, -2.7235),
    c(-78.6760, -2.7235), c(-78.6760, -2.7240)
  )))
)
polygon_test_metrics <- polygon_metrics_from_feature(polygon_test_feature)
add_test("REG008", "Selector display labels contain no missing or blank values", all(!is.na(registry_runtime$display_label) & nzchar(registry_runtime$display_label)), sum(is.na(registry_runtime$display_label) | !nzchar(registry_runtime$display_label)), 0L)
add_test("REG009", "System-choice values and labels contain no missing or blank values", all(!is.na(registry_choices_runtime) & nzchar(registry_choices_runtime)) && all(!is.na(names(registry_choices_runtime)) & nzchar(names(registry_choices_runtime))), sum(is.na(registry_choices_runtime) | !nzchar(registry_choices_runtime)) + sum(is.na(names(registry_choices_runtime)) | !nzchar(names(registry_choices_runtime))), 0L)
add_test("REG010", "Blank startup filters resolve to the complete registry", nrow(blank_filtered_runtime) == nrow(registry_runtime), nrow(blank_filtered_runtime), nrow(registry_runtime))
add_test("REG011", "Specific canton filtering cannot create phantom NA rows", nrow(first_canton_filtered_runtime) > 0L && all(registry_text_or_blank(first_canton_filtered_runtime$canton_display) == first_canton_runtime) && all(is.finite(first_canton_filtered_runtime$latitude)) && all(is.finite(first_canton_filtered_runtime$longitude)), nrow(first_canton_filtered_runtime), "positive rows; selected harmonised canton only; finite coordinates")
add_test("REG012", "Rows with missing administrative labels remain safely selectable", if (restricted_present) sum(missing_admin_runtime) == 5L && all(nzchar(registry_runtime$display_label[missing_admin_runtime])) else all(nzchar(registry_runtime$display_label)), if (restricted_present) sum(missing_admin_runtime) else 0L, if (restricted_present) 5L else 0L)
add_test("REG013", "Map registry rows remain complete and coordinate-valid at startup", nrow(map_rows_runtime) == nrow(registry_runtime) && all(is.finite(map_rows_runtime$latitude)) && all(is.finite(map_rows_runtime$longitude)), nrow(map_rows_runtime), nrow(registry_runtime))
add_test("REG014", "Administrative crosswalk covers every nonblank active source label", all(source_admin_keys_runtime %in% crosswalk_keys_runtime), sum(!source_admin_keys_runtime %in% crosswalk_keys_runtime), 0L)
add_test("REG015", "Institutional canton and parish source values remain preserved", identical(registry_runtime$canton_source, registry_text_or_blank(registry_runtime$canton)) && identical(registry_runtime$parish_source, registry_text_or_blank(registry_runtime$parish)), "source/display separation checked", "source values unchanged")
add_test("REG016", "Harmonised canton display labels have no case/diacritic duplicates", !anyDuplicated(registry_label_key(canton_display_values_runtime)), paste(canton_display_values_runtime, collapse = " | "), "unique normalised keys")
add_test(
  "REG017",
  "Direct reference search is restricted to name/ID and returns only true matches",
  nrow(reference_search_runtime) >= 1L &&
    reference_id_runtime %in% reference_search_runtime$public_id &&
    all(vapply(
      strsplit(normalise_system_search_text(reference_query_runtime), " ", fixed = TRUE)[[1]],
      function(token) all(grepl(
        token,
        normalise_system_search_text(paste(reference_search_runtime$public_id, reference_search_runtime$system_name)),
        fixed = TRUE
      )),
      logical(1)
    )),
  paste(reference_search_runtime$public_id, collapse = " | "),
  paste("contains", reference_id_runtime, "and no unrelated records")
)
add_test("REG018", "Polygon helper calculates positive map-derived crop area and finite centroid", !is.null(polygon_test_metrics) && polygon_test_metrics$area_ha > 0 && all(is.finite(c(polygon_test_metrics$centroid_latitude, polygon_test_metrics$centroid_longitude))), paste(polygon_test_metrics$area_ha, polygon_test_metrics$centroid_latitude, polygon_test_metrics$centroid_longitude, sep = " | "), "positive area and finite centroid")
add_test("REG019", "Harmonised parish display labels have no case/diacritic duplicates", !anyDuplicated(registry_label_key(parish_display_values_runtime)), length(parish_display_values_runtime), "unique normalised keys")
add_test("REG020", "Parish display labels remain unique within every harmonised canton", all(per_canton_parish_unique_runtime), paste(names(per_canton_parish_unique_runtime)[!per_canton_parish_unique_runtime], collapse = " | "), "no duplicated normalised parish label within canton")
add_test("REG021", "Restricted selector exposes 13 harmonised canton choices", if (restricted_present) length(registry_filter_choices(registry_runtime$canton_display)) == 13L else length(registry_filter_choices(registry_runtime$canton_display)) >= 1L, length(registry_filter_choices(registry_runtime$canton_display)), if (restricted_present) 13L else ">=1")
accent_search_fixture_runtime <- data.frame(
  public_id = "AZU-TEST-001",
  system_name = "Sistema de Riego Río Azul",
  stringsAsFactors = FALSE
)
accent_search_runtime <- search_system_registry(accent_search_fixture_runtime, "RIO AZUL", max_results = 25L)
id_search_runtime <- search_system_registry(registry_runtime, reference_id_runtime, max_results = 25L)
registry_name_keys_runtime <- normalise_system_search_text(registry_runtime$system_name)
duplicate_keys_runtime <- unique(registry_name_keys_runtime[duplicated(registry_name_keys_runtime)])
if (restricted_present && length(duplicate_keys_runtime)) {
  duplicate_name_runtime <- registry_runtime[registry_name_keys_runtime == duplicate_keys_runtime[[1]], , drop = FALSE]
} else {
  duplicate_name_runtime <- data.frame(
    public_id = c("AZU-TEST-DUP-001", "AZU-TEST-DUP-002"),
    system_name = c("Sistema de Riego Duplicado", "Sistema de Riego Duplicado"),
    display_label = c(
      "AZU-TEST-DUP-001 — Sistema de Riego Duplicado | Cantón A / Parroquia A",
      "AZU-TEST-DUP-002 — Sistema de Riego Duplicado | Cantón B / Parroquia B"
    ),
    stringsAsFactors = FALSE
  )
}
add_test("REG022", "Direct text search finds the reference system by name", nrow(reference_search_runtime) >= 1L && reference_id_runtime %in% reference_search_runtime$public_id, paste(reference_search_runtime$public_id, collapse = " | "), paste("contains", reference_id_runtime))
add_test("REG023", "Direct name/ID search is accent- and case-insensitive within system names", nrow(accent_search_runtime) == 1L && identical(accent_search_runtime$public_id[[1]], "AZU-TEST-001"), paste(accent_search_runtime$public_id, accent_search_runtime$system_name, sep = " | ", collapse = " || "), "AZU-TEST-001 | Sistema de Riego Río Azul")
add_test("REG024", "Direct text search finds an exact active public/application ID", nrow(id_search_runtime) >= 1L && identical(id_search_runtime$public_id[[1]], reference_id_runtime), paste(id_search_runtime$public_id, collapse = " | "), paste(reference_id_runtime, "first"))
add_test(
  "REG027",
  "Selected reference system resolves to a complete canton and parish",
  nzchar(reference_canton_runtime) && nzchar(reference_parish_runtime) &&
    (!restricted_present || (identical(reference_canton_runtime, "Paute") && identical(reference_parish_runtime, "Tomebamba"))),
  paste(reference_canton_runtime, reference_parish_runtime, sep = " / "),
  if (restricted_present) "Paute / Tomebamba" else "nonblank canton / parish"
)
add_test("REG028", "Selected reference parish is available after rebuilding choices for its canton", reference_parish_runtime %in% reference_parish_choices_runtime, paste(reference_parish_choices_runtime, collapse = " | "), paste("contains", reference_parish_runtime))
add_test("REG025", "Duplicate system names remain disambiguated by application ID and location", nrow(duplicate_name_runtime) >= 2L && length(unique(duplicate_name_runtime$public_id)) == nrow(duplicate_name_runtime) && length(unique(duplicate_name_runtime$display_label)) == nrow(duplicate_name_runtime), paste(duplicate_name_runtime$display_label, collapse = " | "), "at least 2 distinct labelled records")
canton_bounds_runtime <- registry_map_bounds(filter_system_registry(registry_runtime, canton = first_canton_runtime, parish = "__ALL__"))
add_test("REG026", "Canton map bounds are finite and independent of parish filtering", length(canton_bounds_runtime) == 4L && all(is.finite(canton_bounds_runtime)) && canton_bounds_runtime[["lng2"]] > canton_bounds_runtime[["lng1"]] && canton_bounds_runtime[["lat2"]] > canton_bounds_runtime[["lat1"]], paste(canton_bounds_runtime, collapse = " | "), "finite ordered bounds")

summary_json <- jsonlite::fromJSON(file.path("data", "audit", "territorial_selector_summary.json"))
public_selector_summary <- as.data.frame(
  read_csv_utf8(file.path("data", "audit", "territorial_selector_public_summary_v1.csv")),
  stringsAsFactors = FALSE
)
public_metric_value <- function(metric) {
  value <- public_selector_summary$value[public_selector_summary$metric == metric]
  if (length(value) != 1L) return(NA_character_)
  as.character(value[[1]])
}
add_test("AUD001", "Raw institutional registry count is 267", as.integer(summary_json$raw_records) == 267L, summary_json$raw_records, 267L)
add_test("AUD002", "Nonempty coordinate-pair count is 168", as.integer(summary_json$non_missing_coordinate_pairs) == 168L, summary_json$non_missing_coordinate_pairs, 168L)
add_test("AUD003", "Coordinate-valid count is 167", as.integer(summary_json$valid_georeferenced_records) == 167L, summary_json$valid_georeferenced_records, 167L)
add_test("AUD004", "Unnamed coordinate-valid records equal 8", as.integer(summary_json$valid_coordinate_records_missing_system_name) == 8L, summary_json$valid_coordinate_records_missing_system_name, 8L)
add_test("AUD005", "Named selector count equals 159", as.integer(summary_json$selectable_named_records) == 159L, summary_json$selectable_named_records, 159L)
if (restricted_present) {
  ambiguity_audit <- as.data.frame(
    read_csv_utf8(file.path("data", "audit", "territorial_name_ambiguity_audit_v1.csv")),
    stringsAsFactors = FALSE
  )
  ambiguity_present_ok <- nrow(ambiguity_audit) >= 1L &&
    all(c("ambiguity_type", "public_id", "interpretation_note") %in% names(ambiguity_audit))
  combined_parish_count <- sum(ambiguity_audit$ambiguity_type == "combined_parish_source_label")
  combined_parish_ok <- combined_parish_count == 1L &&
    all(nzchar(ambiguity_audit$interpretation_note[ambiguity_audit$ambiguity_type == "combined_parish_source_label"]))
  ambiguity_observed <- nrow(ambiguity_audit)
} else {
  ambiguity_present_ok <- all(c("metric", "value", "public_interpretation") %in% names(public_selector_summary)) &&
    all(nzchar(public_selector_summary$public_interpretation))
  combined_parish_count <- suppressWarnings(as.integer(public_metric_value("combined_parish_source_labels")))
  combined_parish_ok <- identical(combined_parish_count, 1L)
  ambiguity_observed <- paste0(
    "aggregate duplicate-name records=",
    public_metric_value("duplicate_normalised_system_name_records"),
    "; combined-parish labels=",
    public_metric_value("combined_parish_source_labels")
  )
}
add_test("AUD006", "Territorial ambiguity evidence is available at the permitted disclosure level", ambiguity_present_ok, ambiguity_observed, if (restricted_present) "record-level controlled audit" else "non-identifying aggregate summary")
add_test("AUD007", "Combined-parish source labels are not interpreted as multiple intakes", combined_parish_ok, combined_parish_count, 1L)

crop <- as.data.frame(read_csv_utf8(file.path("config", "crop_profiles_daily_v2.csv")), stringsAsFactors = FALSE)
add_test("CROP001", "Crop library contains 12 parameterised profiles", nrow(crop) == 12L, nrow(crop), 12L)
add_test("CROP002", "Four locally anchored crop identities are retained", sum(crop$evidence_level == "A") == 4L, sum(crop$evidence_level == "A"), 4L)
add_test("CROP003", "Superchola potato profile is present", sum(crop$crop_profile_id == "papa_superchola") == 1L, sum(crop$crop_profile_id == "papa_superchola"), 1L)

levels <- as.data.frame(read_csv_utf8(file.path("config", "evidence_levels_v1.csv")), stringsAsFactors = FALSE)
add_test("EVID001", "Evidence-readiness framework contains E0 through E4", identical(levels$level, paste0("E", 0:4)), paste(levels$level, collapse = ","), "E0,E1,E2,E3,E4")
add_test("EVID002", "Evidence framework includes complete Spanish and English descriptions", all(nzchar(levels$label_es)) && all(nzchar(levels$label_en)) && all(nzchar(levels$requirements_es)) && all(nzchar(levels$requirements_en)), "bilingual fields checked", "all nonempty")
add_test("EVID003", "E4 is not claimed by the present prototype", grepl("not claimed", levels$authorized_output_en[levels$level == "E4"], fixed = TRUE), levels$authorized_output_en[levels$level == "E4"], "contains not claimed")

add_test("EVID004", "E1 is a blocked incomplete-evidence state rather than an unimplemented seasonal calculator", !grepl("seasonal|estacional", paste(levels$label_en[levels$level == "E1"], levels$authorized_output_en[levels$level == "E1"]), ignore.case = TRUE), paste(levels$label_en[levels$level == "E1"], levels$authorized_output_en[levels$level == "E1"], sep = " | "), "no seasonal calculator claim")
license_text <- paste(readLines("LICENSE", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
citation_text <- paste(readLines("CITATION.cff", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test(
  "GOV003",
  "Final release carries the author-approved MIT licence without asserting an unminted DOI",
  file.exists("LICENSE") &&
    grepl("MIT License", license_text, fixed = TRUE) &&
    grepl("Copyright (c) 2026 Javier Ávila-Larrea", license_text, fixed = TRUE) &&
    !file.exists("LICENSE_PENDING.md") &&
    grepl("license: MIT", citation_text, fixed = TRUE) &&
    !grepl("^doi:", citation_text, ignore.case = TRUE, perl = TRUE),
  "LICENSE and CITATION.cff inspected",
  "MIT licence; no pending marker; no unminted DOI"
)

initial_presets <- load_initial_state_presets()
add_test("INIT001", "Simple initial-water selector contains four declared presets", nrow(initial_presets) == 4L, nrow(initial_presets), 4L)
add_test("INIT002", "Simple initial-water presets retain the expected identifiers", identical(initial_presets$preset_id, c("wet", "medium", "dry", "very_dry")), paste(initial_presets$preset_id, collapse = ","), "wet,medium,dry,very_dry")
add_test("INIT003", "Simple initial-water presets are explicitly user-declared and unverified", all(initial_presets$provenance_class == "USER_DECLARED") && !any(as.logical(initial_presets$verified)), paste(unique(initial_presets$provenance_class), collapse = " | "), "USER_DECLARED; verified FALSE")
add_test("INIT004", "Simple initial-water presets map to the documented technical modes", identical(initial_presets$initial_mode, c("field_capacity", "fraction_raw", "fraction_raw", "fraction_raw")) && max(abs(initial_presets$initial_value - c(0, 0.50, 0.90, 1.10))) <= 1e-12, paste(initial_presets$initial_mode, initial_presets$initial_value, collapse = " | "), "field_capacity 0; fraction_raw 0.50/0.90/1.10")

soil_reg <- as.data.frame(read_csv_utf8(file.path("config", "soilgrids_layer_register.csv")), stringsAsFactors = FALSE)
add_test("SOIL001", "SoilGrids register covers six standard depth intervals", length(unique(soil_reg$depth_interval)) == 6L, paste(unique(soil_reg$depth_interval), collapse = ","), 6L)
add_test("SOIL002", "Required field-capacity and wilting-point quantiles are complete", sum(soil_reg$required & soil_reg$property_role %in% c("field_capacity", "wilting_point")) == 36L, sum(soil_reg$required & soil_reg$property_role %in% c("field_capacity", "wilting_point")), 36L)

demo <- as.data.frame(read_csv_utf8(file.path("data", "demo", "systems_registry_demo.csv")), stringsAsFactors = FALSE)
add_test("GOV001", "Public fixture is synthetic and uses non-institutional application IDs", all(grepl("synthetic", demo$source_status, fixed = TRUE)) && !any(grepl("^AZU-SR-", demo$public_id)), paste(demo$public_id, collapse = ","), "synthetic IDs only")
gitignore <- paste(readLines(".gitignore", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test("GOV002", "Restricted institutional registry is excluded by .gitignore", grepl("data/restricted", gitignore, fixed = TRUE), "data/restricted", "present")

app_text <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
app_data_text <- paste(readLines(file.path("R", "app_data.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
engine_text <- paste(readLines(file.path("R", "app_engine.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
report_text <- paste(readLines(file.path("R", "app_reporting.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
app_weather_text <- paste(readLines(file.path("R", "app_weather.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test("CODE001", "Shiny app calls the integrated planning engine", grepl("compute_riegoazuay_plan", app_text, fixed = TRUE), "search", "present")
add_test("CODE002", "Engine calls layered turn-constrained scheduling", grepl("plan_weather_ensemble_layered", engine_text, fixed = TRUE), "search", "present")
add_test("CODE003", "Engine creates provenance metadata", grepl("build_provenance_table", engine_text, fixed = TRUE), "search", "present")
add_test("CODE004", "Engine applies the evidence-readiness gate", grepl("assess_evidence_level", engine_text, fixed = TRUE), "search", "present")
add_test("CODE005", "App contains Spanish/English state switching", grepl("language", app_text, fixed = TRUE) && grepl("app_dictionary", paste(readLines(file.path("R", "app_i18n.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"), fixed = TRUE), "search", "present")
add_test("CODE006", "Exports omit the complete restricted registry and retain selected record only", grepl("selected_system_only.csv", report_text, fixed = TRUE) && !grepl("systems_registry_159_restricted.csv", report_text, fixed = TRUE), "search", "selected record only")
add_test("CODE007", "Output includes bilingual HTML reporting", grepl("write_bilingual_html_report", report_text, fixed = TRUE), "search", "present")
add_test("CODE008", "Output includes SHA-256 manifest", grepl("MANIFEST_SHA256.csv", report_text, fixed = TRUE), "search", "present")
public_release_text <- paste(readLines("06_prepare_public_release.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test("CODE009", "ZIP helper resolves the destination directory before changing archive root", grepl("target_dir_abs <- normalizePath", report_text, fixed = TRUE), "search", "present")
add_test("CODE010", "Scenario reproducibility export uses the verified ZIP helper", grepl("write_verified_zip_app(staging, zip_path)", report_text, fixed = TRUE), "search", "present")
add_test("CODE011", "Public/reviewer release uses the verified ZIP helper", grepl("write_verified_zip_app(staging, out_zip)", public_release_text, fixed = TRUE), "search", "present")
add_test("CODE012", "The app does not attach jsonlite and therefore cannot mask Shiny validation", !grepl("library(jsonlite)", app_text, fixed = TRUE), "library(jsonlite)", "absent")
add_test("CODE013", "Map validation is explicitly namespaced to Shiny", grepl("shiny::validate(shiny::need", app_text, fixed = TRUE), "search", "present")
add_test("CODE014", "Administrative filters use harmonised display fields", grepl("registry_filter_choices(registry$canton_display)", app_text, fixed = TRUE) && grepl("registry_filter_choices(x$parish_display)", app_text, fixed = TRUE), "search", "canton_display and parish_display")
add_test("CODE015", "System selector uses a dedicated writable name/ID search field", grepl('textInput(\n            "system_search_query"', app_text, fixed = TRUE) && grepl("search_system_registry", app_text, fixed = TRUE) && grepl('updateSelectInput(\n      session, "system_public_id"', app_text, fixed = TRUE), "search", "writable text search plus filtered results")
add_test("CODE016", "Selecting a system stages canton and parish targets", grepl("observeEvent(input$system_public_id", app_text, fixed = TRUE) && grepl("state$pending_canton <- desired_canton", app_text, fixed = TRUE) && grepl("state$pending_parish <- desired_parish", app_text, fixed = TRUE) && grepl('selected = desired_canton', app_text, fixed = TRUE), "search", "staged administrative synchronisation")
add_test("CODE017", "Permanent Back and Next navigation controls are present", grepl('actionButton("nav_back"', app_text, fixed = TRUE) && grepl('actionButton("nav_next"', app_text, fixed = TRUE) && grepl("wizard-footer", app_text, fixed = TRUE), "search", "nav_back/nav_next in sticky footer")
add_test("CODE018", "Every workflow step exposes an instruction card", grepl('uiOutput("step_instruction_ui")', app_text, fixed = TRUE) && grepl("territory_instruction_title", app_text, fixed = TRUE) && grepl("traceability_instruction_title", app_text, fixed = TRUE), "search", "instruction card for five steps")
add_test("CODE019", "Simple results include a daily action calendar", grepl('uiOutput("daily_plan_ui")', app_text, fixed = TRUE) && grepl("build_daily_action_plan_table", report_text, fixed = TRUE), "search", "daily plan UI and reporting function")
add_test("CODE020", "User-facing Excel export and technical CSV exports are both available", grepl('output$download_user_excel <- downloadHandler', app_text, fixed = TRUE) && grepl("write_user_xlsx_export", report_text, fixed = TRUE) && grepl("daily_action_plan_ES_EN.csv", report_text, fixed = TRUE), "search", "user XLSX plus technical CSV/bundle entries")
add_test("CODE021", "Uncertainty plot and raw diagnostics remain in technical view", grepl('conditionalPanel(\n      "input.technical_view == true"', app_text, fixed = TRUE) && grepl('plotOutput("scenario_range_plot"', app_text, fixed = TRUE), "search", "technical-view boundary")
public_dictionary_values <- c(unname(app_dictionary$es), unname(app_dictionary$en))
weather_public_values <- public_dictionary_values[grepl("meteor|weather|pronóstico|forecast", public_dictionary_values, ignore.case = TRUE)]
provenance_labels <- as.data.frame(read_csv_utf8(file.path("config", "provenance_classes.csv")), stringsAsFactors = FALSE)
public_jargon_values <- c(weather_public_values, provenance_labels$label_es, provenance_labels$label_en)
add_test("CODE022", "Public weather and provenance labels avoid the developer term fixture", !any(grepl("fixture", public_jargon_values, ignore.case = TRUE)), paste(public_jargon_values[grepl("fixture", public_jargon_values, ignore.case = TRUE)], collapse = " | "), "no public fixture wording")
add_test("CODE023", "Spanish and English dictionaries have identical unique keys", identical(names(app_dictionary$es), names(app_dictionary$en)) && !anyDuplicated(names(app_dictionary$es)) && !anyDuplicated(names(app_dictionary$en)), paste(length(app_dictionary$es), length(app_dictionary$en), sep = " / "), "identical unique keys")
i18n_text <- paste(readLines(file.path("R", "app_i18n.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test("CODE024", "Navigation translation avoids the reserved R keyword next", "next_step" %in% names(app_dictionary$es) && !("next" %in% names(app_dictionary$es)) && grepl('tr_app("next_step", l)', app_text, fixed = TRUE) && !grepl('tr_app("next", l)', app_text, fixed = TRUE), paste(c("next_step" %in% names(app_dictionary$es), "next" %in% names(app_dictionary$es)), collapse = " / "), "next_step TRUE; next FALSE")
starter_text <- paste(readLines("INICIAR_AQUI.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
gate_pos <- regexpr('source("00_syntax_gate.R"', starter_text, fixed = TRUE)[[1]]
install_pos <- regexpr('source("00_install_packages.R"', starter_text, fixed = TRUE)[[1]]
add_test("CODE025", "Base-R syntax gate runs before package installation and all test scripts", file.exists("00_syntax_gate.R") && gate_pos > 0L && install_pos > gate_pos, paste(gate_pos, install_pos, sep = " / "), "syntax gate position before installer")
integration_text <- paste(readLines("03_run_integration_smoke_tests.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test(
  "CODE026",
  "Daily-plan builder enforces unique dates and exact decision-horizon coverage",
  grepl("Daily action source contains duplicated dates", report_text, fixed = TRUE) &&
    grepl("Daily action source does not exactly cover", report_text, fixed = TRUE) &&
    grepl("match(expected_chr, observed_chr)", report_text, fixed = TRUE),
  "calendar source guards",
  "duplicate, missing/extra, and canonical-order guards present"
)
add_test(
  "CODE027",
  "Integration calendar comparison is value-based and exposes complete diagnostics",
  !grepl("identical(as.Date(daily_plan$date)", integration_text, fixed = TRUE) &&
    grepl("as.character(observed_daily_dates) == as.character(expected_daily_dates)", integration_text, fixed = TRUE) &&
    grepl("dates=", integration_text, fixed = TRUE),
  "integration assertion audit",
  "value comparison with row count and full date sequence"
)
add_test("CODE028", "Map keeps canton-level bounds when parish filters change", grepl('filter_system_registry(registry, canton = input$canton_filter, parish = "__ALL__")', app_text, fixed = TRUE) && grepl("registry_map_bounds(focus_rows)", app_text, fixed = TRUE), "map source audit", "canton focus ignores active parish")
add_test("CODE029", "Point and guided crop-polygon controls are visible in the simple workflow", grepl('actionButton("activate_point_picker"', app_text, fixed = TRUE) && grepl('actionButton("activate_polygon_draw"', app_text, fixed = TRUE) && grepl('uiOutput("polygon_area_choice_ui")', app_text, fixed = TRUE) && grepl("addDrawToolbar", app_text, fixed = TRUE), "spatial-input audit", "point picker, guided polygon, and explicit area choice")
add_test("CODE030", "Map explains intake, productive point, polygon, and intake-to-point distance", grepl("map_legend_title", app_text, fixed = TRUE) && grepl("addPolylines", app_text, fixed = TRUE) && grepl("intake_productive_distance", app_text, fixed = TRUE), "map communication audit", "legend plus distance line")
add_test("CODE031", "Simple evidence screen prioritises actionable improvements and hides raw source counts", grepl("priority_actions_title", app_text, fixed = TRUE) && grepl("source-summary-grid", app_text, fixed = TRUE) && !grepl("counts <- table(factor", app_text, fixed = TRUE), "evidence UX audit", "actions and core sources; no raw counts")
runtime_register_preflight <- as.data.frame(read_csv_utf8(file.path("config", "runtime_packages_v1.csv")), stringsAsFactors = FALSE)
runtime_packages_preflight <- trimws(as.character(runtime_register_preflight$package))
openxlsx_registered_preflight <- sum(runtime_packages_preflight == "openxlsx", na.rm = TRUE) == 1L
openxlsx_available_preflight <- requireNamespace("openxlsx", quietly = TRUE)
openxlsx_writer_preflight <- grepl("openxlsx::saveWorkbook", report_text, fixed = TRUE)
openxlsx_styling_preflight <- grepl("openxlsx::setColWidths", report_text, fixed = TRUE) &&
  grepl("openxlsx::freezePane", report_text, fixed = TRUE) &&
  grepl("wrapText = TRUE", report_text, fixed = TRUE)
add_test(
  "CODE032",
  "Formatted Excel export package is registered, installed, and used by the styled writer",
  openxlsx_registered_preflight && openxlsx_available_preflight && openxlsx_writer_preflight && openxlsx_styling_preflight,
  paste0(
    "registered=", openxlsx_registered_preflight,
    "; available=", openxlsx_available_preflight,
    "; writer=", openxlsx_writer_preflight,
    "; styling=", openxlsx_styling_preflight
  ),
  "registered=TRUE; available=TRUE; writer=TRUE; styling=TRUE"
)
add_test("CODE033", "Simple download surface prioritises Excel and printable HTML", grepl('downloadButton("download_user_excel"', app_text, fixed = TRUE) && grepl('downloadButton("download_report"', app_text, fixed = TRUE) && grepl("technical-downloads", app_text, fixed = TRUE), "download UX audit", "Excel/HTML primary; raw files technical")
add_test("CODE034", "Complete weather-soil sensitivity range is withheld from the simple metric card", grepl("sensitivity_hidden_simple", app_text, fixed = TRUE) && grepl('plotOutput("scenario_range_plot"', app_text, fixed = TRUE), "results UX audit", "simple boundary plus technical plot")
engine_text <- paste(readLines(file.path("R", "app_engine.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test("CODE035", "Weather and soil preparation use the user-confirmed productive point rather than the intake coordinate", grepl("latitude = inputs$productive_latitude", engine_text, fixed = TRUE) && grepl("longitude = inputs$productive_longitude", engine_text, fixed = TRUE) && length(gregexpr("latitude = inputs$productive_latitude", engine_text, fixed = TRUE)[[1]]) >= 2L && length(gregexpr("longitude = inputs$productive_longitude", engine_text, fixed = TRUE)[[1]]) >= 2L, "engine coordinate-routing audit", "productive coordinates supplied to both soil and weather preparation")
workflow_text <- paste(readLines(file.path(".github", "workflows", "r-static-and-tests.yml"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test("CODE036", "Continuous-integration dependencies include the formatted Excel writer used by integration tests", grepl("any::openxlsx", workflow_text, fixed = TRUE), "GitHub Actions dependency audit", "any::openxlsx present")
add_test("CODE037", "Area input is explicitly crop-specific and excludes whole-property/system area", all(c("area_crop_note", "parcel_scale_statement", "mixed_crop_boundary") %in% names(app_dictionary$es)) && grepl("Área sembrada con el cultivo seleccionado", app_text, fixed = TRUE), "crop-area wording audit", "one crop-specific homogeneous zone")
add_test("CODE038", "Point semantics explicitly forbid an inferred buffer or distributed footprint", grepl("no genera un círculo", app_dictionary$es[["productive_calculation_note"]], fixed = TRUE) && grepl("does not create a circle", app_dictionary$en[["productive_calculation_note"]], fixed = TRUE) && grepl("POINT_NO_FOOTPRINT", engine_text, fixed = TRUE), "point semantics audit", "representative coordinate only")
add_test("CODE039", "Polygon workflow calculates area and centroid and requires explicit source choice", grepl("polygon_metrics_from_feature", app_text, fixed = TRUE) && grepl('\"area_source_choice\"', app_text, fixed = TRUE) && grepl('radioButtons(', app_text, fixed = TRUE) && grepl("effective_area_ha", app_text, fixed = TRUE), "polygon workflow audit", "geometry metrics plus explicit area selection")
add_test("CODE040", "Polygon is bound to the selected crop and invalidated after crop change", grepl("polygon_crop_profile_id", app_text, fixed = TRUE) && grepl("polygon_crop_change_reset", app_text, fixed = TRUE) && grepl("reset_polygon_state(TRUE)", app_text, fixed = TRUE), "polygon-crop consistency audit", "one selected crop per polygon")
add_test("CODE041", "Direct system search haystack contains only public ID and system name", grepl("haystack <- trimws(paste(id_key, system_key))", app_data_text, fixed = TRUE) && !grepl("paste(id_key, system_key, canton_key, parish_key)", app_data_text, fixed = TRUE), "search implementation audit", "ID + system name only")
add_test("CODE042", "Engine enforces crop-specific homogeneous-zone scope and explicit area source", grepl("single_crop_homogeneous_zone", engine_text, fixed = TRUE) && grepl("Area source must be declared or polygon", engine_text, fixed = TRUE) && grepl("Active area does not match the selected polygon-derived area", engine_text, fixed = TRUE), "engine input contract audit", "crop scope and selected area source enforced")
add_test("CODE043", "Reports preserve effective, declared, and polygon-derived crop areas", grepl("effective_crop_area_ha", report_text, fixed = TRUE) && grepl("declared_crop_area_ha", report_text, fixed = TRUE) && grepl("polygon_area_ha", report_text, fixed = TRUE) && grepl("One crop-specific parcel", report_text, fixed = TRUE), "reporting metadata audit", "decision unit and all area sources exported")
add_test("CODE044", "Formatted Excel writer sets widths, wrapping, frozen headers, and styles", grepl("openxlsx::setColWidths", report_text, fixed = TRUE) && grepl("openxlsx::freezePane", report_text, fixed = TRUE) && grepl("wrapText = TRUE", report_text, fixed = TRUE) && grepl("openxlsx::saveWorkbook", report_text, fixed = TRUE), "Excel formatting audit", "readable styled workbook")
add_test("CODE045", "Legacy polygon checkbox was removed from application and tests", !grepl("draw_productive_polygon", app_text, fixed = TRUE) && !grepl("draw_productive_polygon", integration_text, fixed = TRUE), "legacy control audit", "no obsolete checkbox")
add_test("CODE046", "Polygon drawing activation polls until the Leaflet Draw control is available", grepl("var attempts = 24", app_text, fixed = TRUE) && grepl("leaflet-draw-draw-polygon", app_text, fixed = TRUE) && grepl("window.setTimeout(activate, 150)", app_text, fixed = TRUE), "polygon activation audit", "resilient toolbar activation")
add_test("CODE047", "Parish synchronisation waits until the browser confirms the target canton", grepl("if (!is.null(pending_canton) && !identical(active_canton, pending_canton)) return()", app_text, fixed = TRUE) && grepl("pending_parish %in% parish_choices", app_text, fixed = TRUE), "staged observer audit", "wait for canton acknowledgement before parish selection")
add_test("CODE048", "Pending administrative targets clear only after both browser inputs match", grepl("identical(active_canton, pending_canton) && identical(active_parish, pending_parish)", app_text, fixed = TRUE) && grepl("state$pending_canton <- NULL", app_text, fixed = TRUE) && grepl("state$pending_parish <- NULL", app_text, fixed = TRUE), "completion observer audit", "clear after canton and parish confirmation")
add_test("CODE049", "Evidence step is explicitly labelled as evidence and sources in both languages", identical(app_dictionary$es[["step_evidence"]], "3. Revise evidencia y fuentes") && identical(app_dictionary$en[["step_evidence"]], "3. Review evidence and sources"), paste(app_dictionary$es[["step_evidence"]], app_dictionary$en[["step_evidence"]], sep = " | "), "explicit evidence/source labels")
evidence_levels_static <- load_evidence_levels()
add_test("CODE050", "Readiness labels use turn-level rather than fixed-weekly terminology", all(!grepl("semanal|weekly", c(evidence_levels_static$label_es[evidence_levels_static$level %in% c("E2", "E3")], evidence_levels_static$label_en[evidence_levels_static$level %in% c("E2", "E3")]), ignore.case = TRUE)), paste(evidence_levels_static$label_es[evidence_levels_static$level %in% c("E2", "E3")], evidence_levels_static$label_en[evidence_levels_static$level %in% c("E2", "E3")], collapse = " | "), "turn-level labels")
add_test("CODE051", "Scenario displays the non-weekly turn-horizon boundary", grepl('uiOutput("turn_horizon_note_ui")', app_text, fixed = TRUE) && grepl('output$turn_horizon_note_ui <- renderUI', app_text, fixed = TRUE) && grepl("no supone un riego semanal fijo", app_dictionary$es[["turn_horizon_note"]], fixed = TRUE), "turn horizon UI audit", "one opportunity per declared horizon; arbitrary interval")
add_test("CODE052", "User Excel source sheet omits the raw Boolean verified column", grepl('c("Elemento", "Valor activo", "Tipo de fuente")', report_text, fixed = TRUE) && grepl('c("Item", "Active value", "Source type")', report_text, fixed = TRUE) && !grepl('c("Elemento", "Valor activo", "Tipo de fuente", "Verificado")', report_text, fixed = TRUE), "user Excel source columns", "three explanatory columns")
add_test("CODE053", "Technical provenance retains the verified field", grepl('x <- x[, c("input_group", "active_value", "provenance_label", "verified", "calculation_role")]', app_text, fixed = TRUE) && grepl("write_csv_utf8(result$provenance", report_text, fixed = TRUE), "technical provenance audit", "verified retained in technical table/export")
add_test("CODE054", "User Excel sources are built from six explicit core provenance groups with complete labels", grepl('core_ids <- c("productive_location", "productive_area", "weather", "soil", "active_flow", "turn_schedule")', report_text, fixed = TRUE) && grepl("User source export contains an empty or missing display value", report_text, fixed = TRUE), "source-table construction audit", "six explicit non-empty user rows")
add_test("CODE055", "User Excel source values are localised instead of exposing internal tokens", grepl("format_user_source_value_app", report_text, fixed = TRUE) && grepl("user_weather_source_label_app", report_text, fixed = TRUE) && grepl("format_user_turn_schedule_app", report_text, fixed = TRUE) && grepl("no verificado con la organización de riego", report_text, fixed = TRUE), "user source-value formatter audit", "localised human-readable source values")
add_test("CODE056", "User summary localises the productive-location source", grepl("user_location_source_label_app(result$inputs$productive_location_source, language)", report_text, fixed = TRUE), "summary location-source audit", "no raw map_click/manual/polygon_centroid code")

live_wrapper_text <- paste(readLines("AUDITAR_FUENTES_EN_VIVO.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
live_audit_text <- paste(readLines("07_run_live_source_audit.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test("CODE057", "Live-source audit has a dedicated author entry point and implementation script", file.exists("AUDITAR_FUENTES_EN_VIVO.R") && file.exists("07_run_live_source_audit.R"), "live audit scripts", "both present")
add_test("CODE058", "Live weather gate calls the configured deterministic and ensemble workflow and archives receipts", grepl("prepare_live_forecast_ensemble", live_audit_text, fixed = TRUE) && grepl("ensemble_request_receipt.csv", live_audit_text, fixed = TRUE) && grepl("deterministic_request_receipt.csv", live_audit_text, fixed = TRUE), "weather gateway audit", "deterministic + ensemble + receipts")
add_test("CODE059", "Live soil gate uses the application SoilGrids workflow with WCS/WebDAV provenance", grepl('mode = "live_soilgrids"', live_audit_text, fixed = TRUE) && grepl("soilgrids_access_attempts.csv", live_audit_text, fixed = TRUE) && grepl("WCS_GETCOVERAGE", live_audit_text, fixed = TRUE) && grepl("WEBDAV_VRT", live_audit_text, fixed = TRUE), "soil gateway audit", "live SoilGrids with WCS/WebDAV diagnostics")
add_test("CODE060", "Live gate archives session information, tests, metadata, manifest, and a verified ZIP", grepl("sessionInfo.txt", live_audit_text, fixed = TRUE) && grepl("live_source_audit_tests.csv", live_audit_text, fixed = TRUE) && grepl("LIVE_SOURCE_AUDIT_METADATA.csv", live_audit_text, fixed = TRUE) && grepl("write_sha256_manifest", live_audit_text, fixed = TRUE) && grepl("write_verified_zip_app", live_audit_text, fixed = TRUE), "audit evidence package", "session + tests + metadata + manifest + ZIP")
add_test("CODE061", "Live audit is kept separate from the retrospective experiment", !grepl("2304", live_audit_text, fixed = TRUE) && !grepl("retrospective", live_audit_text, ignore.case = TRUE) && grepl("does not rerun the retrospective experiment", live_wrapper_text, fixed = TRUE), "separation audit", "no retrospective execution in live gate")
add_test("CODE062", "Author live-audit entry point runs syntax, package, and static gates before network acquisition", grepl('source("00_syntax_gate.R"', live_wrapper_text, fixed = TRUE) && grepl('source("00_install_packages.R"', live_wrapper_text, fixed = TRUE) && grepl('source("01_static_preflight.R"', live_wrapper_text, fixed = TRUE) && grepl('source("07_run_live_source_audit.R"', live_wrapper_text, fixed = TRUE), "entry-point order audit", "syntax -> packages -> preflight -> live audit")

add_test("CODE063", "Live ensemble workflow distinguishes all, retained, and excluded-member coverage", grepl("coverage_all = coverage_all", app_weather_text, fixed = TRUE) && grepl("coverage = coverage_retained", app_weather_text, fixed = TRUE) && grepl("excluded_incomplete_members = incomplete_members", app_weather_text, fixed = TRUE), "ensemble coverage contract", "all/retained/excluded coverage preserved separately")
add_test("CODE064", "Release gate evaluates retained coverage rather than rejected upstream members", grepl("nrow(retained_coverage) == length(retained_ids)", live_audit_text, fixed = TRUE) && grepl("setequal(as.character(retained_coverage$group), retained_ids)", live_audit_text, fixed = TRUE), "WTH008 contract audit", "retained members only")
add_test("CODE065", "Retained-member horizon audit checks character dates, duplicates, missing dates, and extras", grepl("audit_retained_member_horizons", app_weather_text, fixed = TRUE) && grepl("duplicate_date_count", app_weather_text, fixed = TRUE) && grepl("missing_dates", app_weather_text, fixed = TRUE) && grepl("extra_dates", app_weather_text, fixed = TRUE) && !grepl("identical(sort(unique(x)), expected_dates)", live_audit_text, fixed = TRUE), "horizon audit implementation", "explicit eight-day value contract")
add_test("CODE066", "Live gate archives per-member horizon diagnostics", grepl("release_gate_retained_member_horizons.csv", live_audit_text, fixed = TRUE), "horizon diagnostic audit", "CSV archived")
add_test("CODE067", "Live gate verifies exclusion and cross-object retained-member count consistency", grepl('add_check("WTH016"', live_audit_text, fixed = TRUE) && grepl('add_check("WTH017"', live_audit_text, fixed = TRUE) && grepl("excluded_not_retained_ok", live_audit_text, fixed = TRUE) && grepl("member_count_consistent_ok", live_audit_text, fixed = TRUE), "retained/excluded audit", "WTH016 and WTH017 present")
add_test("CODE068", "Weather and soil warning streams are captured in the evidence bundle", grepl("LIVE_WEATHER_WARNINGS.txt", live_audit_text, fixed = TRUE) && grepl("LIVE_SOIL_WARNINGS.txt", live_audit_text, fixed = TRUE) && grepl('add_check("ARC003"', live_audit_text, fixed = TRUE), "warning evidence audit", "two warning logs and archive check")


freeze_text <- paste(readLines("05_freeze_environment.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
legacy_inventory_subset <- 'utils::installed.packages()[, c("Package", "Version", "LibPath", "Repository"), drop = FALSE]'
add_test(
  "CODE069",
  "Environment freeze builds a lockfile-driven package inventory without assuming optional installed.packages columns",
  grepl("build_locked_package_inventory", freeze_text, fixed = TRUE) &&
    grepl("VersionMatchesLock", freeze_text, fixed = TRUE) &&
    grepl("LockRepository", freeze_text, fixed = TRUE) &&
    grepl("one complete version-matched row per lockfile package", freeze_text, fixed = TRUE) &&
    !grepl(legacy_inventory_subset, freeze_text, fixed = TRUE),
  "lockfile-driven inventory contract audit",
  "schema-safe inventory with version matching and no mandatory Repository column"
)


release_packaging_text <- paste(readLines("06_prepare_public_release.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
release_gate_text <- paste(readLines("09_run_release_candidate_gate.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
public_verify_text <- paste(readLines("VERIFY_PUBLIC_PACKAGE.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test(
  "CODE070",
  "Public package generator excludes record-level territorial audits while reviewer packaging retains controlled evidence",
  grepl("territorial_selector_audit_final_v1", release_packaging_text, fixed = TRUE) &&
    grepl("territorial_name_ambiguity_audit_v1", release_packaging_text, fixed = TRUE) &&
    grepl("if (!include_restricted)", release_packaging_text, fixed = TRUE),
  "public exclusion-pattern audit",
  "two detailed audit files excluded only from public package"
)
add_test(
  "CODE071",
  "Public package generator recreates a safe restricted-data notice and non-identifying selector summary",
  grepl("write_public_restricted_notice", release_packaging_text, fixed = TRUE) &&
    grepl("territorial_selector_public_summary_v1.csv", release_packaging_text, fixed = TRUE) &&
    grepl("record-level territorial audits are intentionally absent", release_packaging_text, fixed = TRUE),
  "public safe-assets audit",
  "safe README, aggregate summary, and explicit notice"
)
add_test(
  "CODE072",
  "Distributed package inventories omit author-workstation paths and regenerate the reproducibility manifest",
  grepl("sanitize_distributed_inventory", release_packaging_text, fixed = TRUE) &&
    !grepl('"LibPath"', release_packaging_text, fixed = TRUE) &&
    !grepl('"PackagePath"', release_packaging_text, fixed = TRUE) &&
    grepl("refresh_distributed_reproducibility_manifest", release_packaging_text, fixed = TRUE) &&
    grepl("assert_no_local_workstation_paths", release_packaging_text, fixed = TRUE),
  "distributed-inventory sanitisation audit",
  "local path columns omitted and manifest regenerated"
)
add_test(
  "CODE073",
  "Public package has a dedicated non-confidential self-test that does not open the app or call live sources",
  file.exists("VERIFY_PUBLIC_PACKAGE.R") &&
    grepl('source("00_syntax_gate.R"', public_verify_text, fixed = TRUE) &&
    grepl('source("01_static_preflight.R"', public_verify_text, fixed = TRUE) &&
    grepl('source("02_run_scientific_tests.R"', public_verify_text, fixed = TRUE) &&
    grepl('source("03_run_integration_smoke_tests.R"', public_verify_text, fixed = TRUE) &&
    !grepl('source("04_run_app.R"', public_verify_text, fixed = TRUE) &&
    !grepl('source("07_run_live_source_audit.R"', public_verify_text, fixed = TRUE),
  "public self-test entry-point audit",
  "syntax + preflight + scientific + integration only"
)
add_test(
  "CODE074",
  "Release-candidate gate rejects forbidden public territorial files and exact institutional coordinate fingerprints",
  grepl("territorial_selector_audit_final_v1.csv", release_gate_text, fixed = TRUE) &&
    grepl("territorial_name_ambiguity_audit_v1.csv", release_gate_text, fixed = TRUE) &&
    grepl("scan_restricted_coordinate_fingerprints", release_gate_text, fixed = TRUE),
  "public confidentiality gate audit",
  "forbidden files and exact coordinate fingerprint scan"
)
add_test(
  "CODE075",
  "Release-candidate gate executes and verifies the extracted public package",
  grepl("run_public_package_self_test", release_gate_text, fixed = TRUE) &&
    grepl("PUBLIC_PACKAGE_SELF_TEST_LOG.txt", release_gate_text, fixed = TRUE) &&
    grepl("public_self_test", release_gate_text, fixed = TRUE),
  "public self-test gate audit",
  "extracted package is executed and all three suites are checked"
)
add_test(
  "CODE076",
  "RC4 confidentiality correction and Methods/data-governance note remain archived",
  file.exists(file.path("docs", "PATCH_RC4_PUBLIC_PACKAGE_CONFIDENTIALITY.md")) &&
    file.exists(file.path("docs", "RC4_METHODS_DATA_GOVERNANCE_NOTE.md")),
  "RC4 documentation audit",
  "historical privacy patch and consistency note present"
)

integration_contract_text <- paste(readLines("03_run_integration_smoke_tests.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
add_test(
  "CODE077",
  "Public and restricted integration tests validate the soil label for the active registry mode",
  file.exists(file.path("docs", "PATCH_RC5_PUBLIC_INTEGRATION_SOIL_SOURCE_CONTRACT.md")) &&
    grepl("expected_soil_source_es <- switch", integration_contract_text, fixed = TRUE) &&
    grepl("expected_soil_source_en <- switch", integration_contract_text, fixed = TRUE) &&
    grepl("fao_reference = \"Suelo general de referencia FAO\"", integration_contract_text, fixed = TRUE) &&
    grepl("fao_reference = \"General FAO reference soil\"", integration_contract_text, fixed = TRUE) &&
    grepl("grepl(expected_soil_source_es", integration_contract_text, fixed = TRUE) &&
    grepl("grepl(expected_soil_source_en", integration_contract_text, fixed = TRUE),
  "mode-aware soil-source integration contract",
  "restricted=SoilGrids; public synthetic registry=FAO reference"
)

all_tests <- dplyr::bind_rows(tests)
dir.create(file.path("outputs", "tests"), recursive = TRUE, showWarnings = FALSE)
write_csv_utf8(all_tests, file.path("outputs", "tests", "static_preflight_tests.csv"))
print_test_summary(all_tests, "RESULTADO GENERAL: EL PREVUELO ESTÁTICO REGISTRÓ FALLOS.")
cat("RESULTADO GENERAL: PREVUELO ESTÁTICO SUPERADO.\n")
if (identical(Sys.getenv("RIEGOAZUAY_LIVE_GATE", unset = ""), "1")) {
  cat('Next step: source("07_run_live_source_audit.R")\n')
} else {
  cat('Next step: source("02_run_scientific_tests.R")\n')
}
