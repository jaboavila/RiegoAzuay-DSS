source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "profile_library.R"), encoding = "UTF-8")
source(file.path("R", "fao56_daily_balance.R"), encoding = "UTF-8")
source(file.path("R", "layered_soil_balance.R"), encoding = "UTF-8")
source(file.path("R", "irrigation_scheduler.R"), encoding = "UTF-8")
source(file.path("R", "layered_irrigation_scheduler.R"), encoding = "UTF-8")
source(file.path("R", "evidence_gate.R"), encoding = "UTF-8")
source(file.path("R", "flow_measurement_assistant.R"), encoding = "UTF-8")
source(file.path("R", "scientific_tests.R"), encoding = "UTF-8")

cat("\n=== RIEGOAZUAY DSS: OFFLINE SCIENTIFIC REGRESSION TESTS ===\n")
dir.create(file.path("outputs", "tests"), recursive = TRUE, showWarnings = FALSE)
base_tests <- run_all_scientific_tests()

profile <- get_crop_profile(load_crop_profiles_daily(), "papa_superchola")
layers <- data.frame(
  soil_scenario = "central",
  depth_interval = c("0-5cm", "5-15cm", "15-30cm", "30-60cm"),
  depth_top_m = c(0, 0.05, 0.15, 0.30),
  depth_bottom_m = c(0.05, 0.15, 0.30, 0.60),
  theta_fc = c(0.352, 0.360, 0.361, 0.344),
  theta_wp = c(0.184, 0.191, 0.192, 0.186),
  stringsAsFactors = FALSE
)
weather <- data.frame(
  date = seq(as.Date("2026-11-01"), as.Date("2026-11-07"), by = "day"),
  eto_mm_day = c(3.4, 3.6, 3.8, 3.7, 3.5, 3.2, 3.3),
  precip_mm_day = c(0, 0, 2, 0, 0, 8, 0),
  stringsAsFactors = FALSE
)
layered <- simulate_daily_balance_layered(
  weather = weather,
  profile_row = profile,
  soil_layers = layers,
  days_after_planting_start = 60,
  initial_depletion_mm = 0.90 * (profile$p_table[[1]] * layered_taw_mm(layers, build_root_depth_curve(profile)[[60]])),
  delivery_efficiency = 0.75,
  adjust_p_for_etc = TRUE,
  runoff_fraction = 0,
  subsoil_water_mode = "field_capacity"
)
turn <- plan_weekly_irrigation_layered(
  weather = weather,
  profile_row = profile,
  soil_layers = layers,
  days_after_planting_start = 60,
  initial_depletion_mm = layered$depletion_start_mm[[1]],
  area_ha = 0.25,
  delivery_efficiency = 0.75,
  allowed_dates = as.Date("2026-11-01"),
  flow_lps = 2,
  max_hours_by_date = c("2026-11-01" = 6),
  next_turn_by_date = c("2026-11-01" = "2026-11-08"),
  reporting_horizon_end_date = as.Date("2026-11-07"),
  runoff_fraction = 0,
  subsoil_water_mode = "field_capacity"
)
layer_tests <- dplyr::bind_rows(
  new_test_row("LAY01", "Layered TAW at 0.40 m is positive", layered_taw_mm(layers, 0.40) > 0, layered_taw_mm(layers, 0.40), ">0"),
  new_test_row("LAY02", "Layered daily mass-balance residual is negligible", max(abs(layered$balance_residual_mm), na.rm = TRUE) <= 1e-10, max(abs(layered$balance_residual_mm), na.rm = TRUE), "<=1e-10"),
  new_test_row("LAY03", "Layered depletion remains nonnegative", all(layered$depletion_end_mm >= -1e-10), min(layered$depletion_end_mm), ">=0"),
  new_test_row("LAY04", "Turn capacity equals 43.2 m3 at 2 L/s for 6 h", nrow(turn$events) == 1L && abs(turn$events$turn_capacity_m3[[1]] - 43.2) <= 1e-9, if (nrow(turn$events)) turn$events$turn_capacity_m3[[1]] else NA_real_, 43.2),
  new_test_row("LAY05", "Turn event reports nonnegative unmet volume", nrow(turn$events) == 1L && turn$events$unmet_volume_m3[[1]] >= 0, if (nrow(turn$events)) turn$events$unmet_volume_m3[[1]] else NA_real_, ">=0"),
  new_test_row("LAY06", "Turn event preserves next-turn coverage", nrow(turn$events) == 1L && isTRUE(turn$events$weather_covers_to_next_turn[[1]]), if (nrow(turn$events)) turn$events$weather_covers_to_next_turn[[1]] else NA, TRUE)
)

all_tests <- dplyr::bind_rows(base_tests, layer_tests)
write_csv_utf8(all_tests, file.path("outputs", "tests", "offline_scientific_tests.csv"))
print_test_summary(all_tests, "RESULTADO GENERAL: LAS PRUEBAS CIENTÍFICAS OFFLINE REGISTRARON FALLOS.")
cat("RESULTADO GENERAL: PRUEBAS CIENTÍFICAS OFFLINE SUPERADAS.\n")
cat('Next step: source("03_run_integration_smoke_tests.R")\n')
