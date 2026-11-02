run_fao56_example36_tests <- function() {
  x <- as.data.frame(read_csv_utf8(file.path("data", "reference", "fao56_example36_expected.csv")))
  tests <- list(); k <- 1L
  for (i in seq_len(nrow(x))) {
    taw <- calc_taw_mm(x$theta_fc[[i]], x$theta_wp[[i]], x$root_depth_m[[i]])
    raw <- calc_raw_mm(taw, x$p[[i]])
    tests[[k]] <- new_test_row(paste0("FAO36_TAW_", i), paste(x$crop[[i]], x$soil[[i]], "TAW"),
                               abs(taw - x$taw_expected_mm[[i]]) <= 1e-9, taw, x$taw_expected_mm[[i]]); k <- k + 1L
    tests[[k]] <- new_test_row(paste0("FAO36_RAW_", i), paste(x$crop[[i]], x$soil[[i]], "RAW"),
                               abs(raw - x$raw_expected_mm[[i]]) <= 1e-9, raw, x$raw_expected_mm[[i]]); k <- k + 1L
  }
  do.call(rbind, tests)
}

run_fao56_example37_tests <- function() {
  expected <- as.data.frame(read_csv_utf8(file.path("data", "reference", "fao56_example37_expected.csv")))
  weather <- data.frame(
    date = seq.Date(as.Date("2001-01-01"), by = "day", length.out = 10),
    eto_mm_day = rep(5, 10), precip_mm_day = rep(0, 10)
  )
  profile <- data.frame(
    cycle_days = 100, initial_days = 1, development_days = 1, mid_days = 98, late_days = 0,
    kc_initial = 1.2, kc_mid = 1.2, kc_end = 1.2,
    zr_initial_m = 0.8, zr_scheduling_m = 0.8, p_table = 0.4,
    stringsAsFactors = FALSE
  )
  soil <- data.frame(theta_fc = 0.32, theta_wp = 0.12)
  observed <- simulate_daily_balance(weather, profile, soil, 1, 55,
                                     delivery_efficiency = 1,
                                     adjust_p_for_etc = FALSE,
                                     subsoil_water_mode = "field_capacity")
  tests <- list(); k <- 1L
  for (i in seq_len(10)) {
    tests[[k]] <- new_test_row(paste0("FAO37_DRSTART_", i), paste("Day", i, "start depletion"),
                               abs(observed$depletion_start_mm[[i]] - expected$depletion_start_expected_mm[[i]]) <= 0.11,
                               round(observed$depletion_start_mm[[i]], 3), expected$depletion_start_expected_mm[[i]]); k <- k + 1L
    tests[[k]] <- new_test_row(paste0("FAO37_KS_", i), paste("Day", i, "Ks"),
                               abs(observed$ks[[i]] - expected$ks_expected[[i]]) <= 0.006,
                               round(observed$ks[[i]], 4), expected$ks_expected[[i]]); k <- k + 1L
    tests[[k]] <- new_test_row(paste0("FAO37_ETCADJ_", i), paste("Day", i, "adjusted ETc"),
                               abs(observed$etc_adjusted_mm_day[[i]] - expected$etc_adjusted_expected_mm_day[[i]]) <= 0.11,
                               round(observed$etc_adjusted_mm_day[[i]], 3), expected$etc_adjusted_expected_mm_day[[i]]); k <- k + 1L
    tests[[k]] <- new_test_row(paste0("FAO37_DREND_", i), paste("Day", i, "end depletion"),
                               abs(observed$depletion_end_mm[[i]] - expected$depletion_end_expected_mm[[i]]) <= 0.11,
                               round(observed$depletion_end_mm[[i]], 3), expected$depletion_end_expected_mm[[i]]); k <- k + 1L
  }
  do.call(rbind, tests)
}

run_root_expansion_tests <- function() {
  weather <- data.frame(
    date = as.Date(c("2026-01-01", "2026-01-02")),
    eto_mm_day = c(0, 0), precip_mm_day = c(0, 0)
  )
  profile <- data.frame(
    cycle_days = 3, initial_days = 1, development_days = 2, mid_days = 0, late_days = 0,
    kc_initial = 1, kc_mid = 1, kc_end = 1,
    zr_initial_m = 0.2, zr_scheduling_m = 0.4, p_table = 0.5,
    stringsAsFactors = FALSE
  )
  soil <- data.frame(theta_fc = 0.30, theta_wp = 0.10)
  fc <- simulate_daily_balance(weather, profile, soil, 1, 10, subsoil_water_mode = "field_capacity")
  measured <- simulate_daily_balance(weather, profile, soil, 1, 10,
                                     subsoil_water_mode = "measured_theta", subsoil_theta_value = 0.20)
  relative <- simulate_daily_balance(weather, profile, soil, 1, 10,
                                     subsoil_water_mode = "same_relative_depletion")
  dplyr::bind_rows(
    new_test_row("ROOT01", "Field-capacity assumption adds zero depletion", abs(fc$root_expansion_depletion_adjustment_mm[[2]]) <= 1e-12,
                 fc$root_expansion_depletion_adjustment_mm[[2]], 0),
    new_test_row("ROOT02", "Measured subsoil theta adds the expected depletion", abs(measured$root_expansion_depletion_adjustment_mm[[2]] - 10) <= 1e-9,
                 measured$root_expansion_depletion_adjustment_mm[[2]], 10),
    new_test_row("ROOT03", "Same-relative-depletion mode preserves fractional depletion", abs(relative$root_expansion_depletion_adjustment_mm[[2]] - 5) <= 1e-9,
                 relative$root_expansion_depletion_adjustment_mm[[2]], 5),
    new_test_row("ROOT04", "Root-depth increment is explicit", abs(measured$root_depth_increment_m[[2]] - 0.1) <= 1e-12,
                 measured$root_depth_increment_m[[2]], 0.1),
    new_test_row("ROOT05", "Measured-theta start depletion includes root expansion", abs(measured$depletion_start_mm[[2]] - 20) <= 1e-9,
                 measured$depletion_start_mm[[2]], 20),
    new_test_row("ROOT06", "Daily mass-balance residual remains negligible", max(abs(measured$balance_residual_mm)) <= 1e-10,
                 max(abs(measured$balance_residual_mm)), "<=1e-10")
  )
}

run_flow_assistant_tests <- function() {
  reps <- read_csv_utf8(file.path("data", "fixtures", "flow_volumetric_demo.csv"))
  context_ok <- read_csv_utf8(file.path("data", "fixtures", "flow_measurement_context_demo.csv"))
  context_bad <- read_csv_utf8(file.path("data", "fixtures", "flow_measurement_context_incomplete.csv"))
  vol <- summarise_volumetric_measurement(reps, context_ok)
  vol_incomplete <- summarise_volumetric_measurement(reps, context_bad)
  flt <- summarise_float_measurement(read_csv_utf8(file.path("data", "fixtures", "flow_float_demo.csv")))
  meter_unverified <- summarise_meter_difference(100, 101.2, 600, meter_validated = FALSE)
  meter_verified <- summarise_meter_difference(100, 101.2, 600, meter_validated = TRUE)
  dplyr::bind_rows(
    new_test_row("FLOW01", "Volumetric median flow", abs(vol$summary$median_flow_lps[[1]] - 2) <= 1e-12,
                 vol$summary$median_flow_lps[[1]], 2),
    new_test_row("FLOW02", "Volumetric repetitions show high repeatability", vol$summary$cv_pct[[1]] < 2,
                 round(vol$summary$cv_pct[[1]], 3), "< 2%"),
    new_test_row("FLOW03", "Complete volumetric context is recognized", isTRUE(vol$summary$context_complete[[1]]),
                 vol$summary$context_complete[[1]], TRUE),
    new_test_row("FLOW04", "Complete context plus repeatability verifies operational flow",
                 isTRUE(vol$summary$operational_flow_verified[[1]]),
                 vol$summary$operational_flow_verified[[1]], TRUE),
    new_test_row("FLOW05", "Incomplete context cannot verify operational flow",
                 !isTRUE(vol_incomplete$summary$operational_flow_verified[[1]]),
                 vol_incomplete$summary$operational_flow_verified[[1]], FALSE),
    new_test_row("FLOW06", "Incomplete context records missing fields",
                 nzchar(vol_incomplete$summary$context_missing_fields[[1]]),
                 vol_incomplete$summary$context_missing_fields[[1]], "nonempty"),
    new_test_row("FLOW07", "Float-method median flow is approximately 36 L/s", abs(flt$summary$median_flow_lps[[1]] - 36) <= 0.1,
                 round(flt$summary$median_flow_lps[[1]], 3), 36),
    new_test_row("FLOW08", "Unverified meter remains conditional", !meter_unverified$operational_flow_verified[[1]],
                 meter_unverified$operational_flow_verified[[1]], FALSE),
    new_test_row("FLOW09", "Validated meter can verify operational flow", meter_verified$operational_flow_verified[[1]],
                 meter_verified$operational_flow_verified[[1]], TRUE),
    new_test_row("FLOW10", "Flow-volume-time conversions are reciprocal",
                 abs(hours_required_for_volume(turn_volume_m3(2, 4), 2) - 4) <= 1e-12,
                 hours_required_for_volume(turn_volume_m3(2, 4), 2), 4)
  )
}

run_balance_invariant_tests <- function(profiles, soils) {
  weather <- data.frame(
    date = seq.Date(as.Date("2026-10-26"), by = "day", length.out = 7),
    eto_mm_day = c(3.4, 3.6, 3.8, 3.7, 3.5, 3.2, 3.3),
    precip_mm_day = c(0, 0, 2, 0, 0, 8, 0)
  )
  profile <- get_crop_profile(profiles, "papa_superchola")
  soil <- get_soil_profile(soils, "medium_silt")
  z <- simulate_daily_balance(weather, profile, soil, 60, 20, 0.75,
                              subsoil_water_mode = "field_capacity")
  dplyr::bind_rows(
    new_test_row("BAL01", "Depletion is nonnegative", all(z$depletion_end_mm >= -1e-12), min(z$depletion_end_mm), ">=0"),
    new_test_row("BAL02", "Depletion does not exceed TAW", all(z$depletion_end_mm <= z$taw_mm + 1e-12),
                 max(z$depletion_end_mm - z$taw_mm), "<=0"),
    new_test_row("BAL03", "Ks stays in [0,1]", all(z$ks >= 0 & z$ks <= 1), range(z$ks), "[0,1]"),
    new_test_row("BAL04", "Stage labels are present", all(nzchar(z$crop_stage_es)), paste(unique(z$crop_stage_es), collapse = ", "), "nonempty"),
    new_test_row("BAL05", "Root depth is nondecreasing", all(diff(z$root_depth_m) >= -1e-12), range(diff(z$root_depth_m)), ">=0"),
    new_test_row("BAL06", "Balance residual is negligible", max(abs(z$balance_residual_mm)) <= 1e-10,
                 max(abs(z$balance_residual_mm)), "<=1e-10")
  )
}

run_weekly_planner_tests <- function(profiles, soils) {
  weather <- as.data.frame(read_csv_utf8(file.path("data", "fixtures", "weather_to_next_turn_ensemble_demo.csv")))
  weather$date <- as.Date(weather$date)
  weather_short <- as.data.frame(read_csv_utf8(file.path("data", "fixtures", "weather_7day_ensemble_demo.csv")))
  weather_short$date <- as.Date(weather_short$date)
  profile <- get_crop_profile(profiles, "papa_superchola")
  soil <- get_soil_profile(soils, "medium_silt")
  allowed <- as.Date("2026-10-28")
  hours_map <- c("2026-10-28" = 6)
  next_map <- c("2026-10-28" = "2026-11-04")
  z <- plan_weather_ensemble(
    weather, profile, soil,
    days_after_planting_start = 60,
    initial_depletion_mm = 20,
    area_ha = 0.25,
    delivery_efficiency = 0.75,
    allowed_dates = allowed,
    flow_lps = 2,
    max_hours_by_date = hours_map,
    next_turn_by_date = next_map,
    reporting_horizon_end_date = as.Date("2026-11-01"),
    subsoil_water_mode = "field_capacity"
  )
  z_short <- plan_weather_ensemble(
    weather_short, profile, soil,
    days_after_planting_start = 60,
    initial_depletion_mm = 20,
    area_ha = 0.25,
    delivery_efficiency = 0.75,
    allowed_dates = allowed,
    flow_lps = 2,
    max_hours_by_date = hours_map,
    next_turn_by_date = next_map,
    reporting_horizon_end_date = as.Date("2026-11-01"),
    subsoil_water_mode = "field_capacity"
  )
  s <- z$scenario_summary
  central <- s[s$weather_scenario == "central", ]
  dry <- s[s$weather_scenario == "dry", ]
  wet <- s[s$weather_scenario == "wet", ]
  all_traces <- do.call(rbind, lapply(z$plans, function(q) q$daily))
  all_events <- do.call(rbind, lapply(z$plans, function(q) q$events))
  short_events <- do.call(rbind, lapply(z_short$plans, function(q) q$events))
  dplyr::bind_rows(
    new_test_row("PLAN01", "Three weather scenarios are evaluated", nrow(s) == 3L, nrow(s), 3),
    new_test_row("PLAN02", "Central requirement reproduces next-turn fixture", abs(central$required_volume_m3[[1]] - 32.995015) <= 0.002,
                 round(central$required_volume_m3[[1]], 6), 32.995015),
    new_test_row("PLAN03", "Wet scenario requires less water than central", wet$required_volume_m3[[1]] < central$required_volume_m3[[1]],
                 wet$required_volume_m3[[1]], paste("<", central$required_volume_m3[[1]])),
    new_test_row("PLAN04", "Dry scenario requires more water than central", dry$required_volume_m3[[1]] > central$required_volume_m3[[1]],
                 dry$required_volume_m3[[1]], paste(">", central$required_volume_m3[[1]])),
    new_test_row("PLAN05", "Dry scenario exceeds the 6 h turn capacity", dry$any_capacity_limit[[1]], dry$any_capacity_limit[[1]], TRUE),
    new_test_row("PLAN06", "Central scenario is feasible within the turn and through next turn",
                 !central$any_capacity_limit[[1]] && !central$any_stress[[1]] && isTRUE(central$covers_to_next_turn_after_delivery[[1]]),
                 paste(central$any_capacity_limit[[1]], central$any_stress[[1]], central$covers_to_next_turn_after_delivery[[1]]),
                 "FALSE FALSE TRUE"),
    new_test_row("PLAN07", "Envelope confidence is low when one scenario conflicts", z$envelope$confidence[[1]] == "LOW",
                 z$envelope$confidence[[1]], "LOW"),
    new_test_row("PLAN08", "Dry deliverable volume is capped at turn capacity", abs(dry$deliverable_volume_m3[[1]] - 43.2) <= 1e-8,
                 dry$deliverable_volume_m3[[1]], 43.2),
    new_test_row("PLAN09", "Dry deficit equals required minus deliverable", abs(dry$unmet_volume_m3[[1]] - (dry$required_volume_m3[[1]] - 43.2)) <= 1e-8,
                 dry$unmet_volume_m3[[1]], dry$required_volume_m3[[1]] - 43.2),
    new_test_row("PLAN10", "Central deficit is zero", abs(central$unmet_volume_m3[[1]]) <= 1e-10,
                 central$unmet_volume_m3[[1]], 0),
    new_test_row("PLAN11", "All extended daily scenario traces are retained", nrow(all_traces) == 27L,
                 nrow(all_traces), 27),
    new_test_row("PLAN12", "All scenario events retain scenario labels", all(c("dry", "central", "wet") %in% all_events$weather_scenario),
                 paste(unique(all_events$weather_scenario), collapse = ","), "dry,central,wet"),
    new_test_row("PLAN13", "Event output contains required-deliverable-deficit fields",
                 all(c("required_volume_m3", "deliverable_volume_m3", "unmet_volume_m3", "unmet_hours") %in% names(all_events)),
                 paste(intersect(c("required_volume_m3", "deliverable_volume_m3", "unmet_volume_m3", "unmet_hours"), names(all_events)), collapse = ","),
                 "all fields"),
    new_test_row("PLAN14", "Extended weather reaches the day before the next turn", all(all_events$weather_covers_to_next_turn),
                 paste(all_events$weather_covers_to_next_turn, collapse = ","), "TRUE"),
    new_test_row("PLAN15", "Next turn is recorded as 2026-11-04", all(as.Date(all_events$next_turn_date) == as.Date("2026-11-04")),
                 paste(unique(all_events$next_turn_date), collapse = ","), "2026-11-04"),
    new_test_row("PLAN16", "Coverage validation ends on the day before the next turn", all(as.Date(all_events$coverage_end_date) == as.Date("2026-11-03")),
                 paste(unique(all_events$coverage_end_date), collapse = ","), "2026-11-03"),
    new_test_row("PLAN17", "Short weather horizon cannot claim next-turn coverage", all(!short_events$weather_covers_to_next_turn),
                 paste(short_events$weather_covers_to_next_turn, collapse = ","), "FALSE"),
    new_test_row("PLAN18", "Short horizon labels coverage as incomplete", all(short_events$coverage_basis == "INCOMPLETE_TO_NEXT_TURN"),
                 paste(unique(short_events$coverage_basis), collapse = ","), "INCOMPLETE_TO_NEXT_TURN")
  )
}

run_evidence_gate_tests <- function() {
  e2_assumption <- assess_evidence_level(TRUE, TRUE, TRUE, TRUE, TRUE, "user_estimate",
                                         flow_quality = "B_VERIFIED_VOLUMETRIC",
                                         flow_operational_verified = TRUE,
                                         has_turn_schedule = TRUE,
                                         community_aggregation = FALSE,
                                         subsoil_water_evidence = "field_capacity_assumption")
  e2_context <- assess_evidence_level(TRUE, TRUE, TRUE, TRUE, TRUE, "measured_theta",
                                      flow_quality = "B_HIGH_REPEATABILITY",
                                      flow_operational_verified = FALSE,
                                      has_turn_schedule = TRUE,
                                      community_aggregation = FALSE,
                                      subsoil_water_evidence = "measured_theta")
  e2_no_history <- assess_evidence_level(TRUE, TRUE, TRUE, TRUE, TRUE, "measured_theta",
                              flow_quality = "B_VERIFIED_VOLUMETRIC",
                              flow_operational_verified = TRUE,
                              has_turn_schedule = TRUE,
                              community_aggregation = FALSE,
                              subsoil_water_evidence = "measured_theta",
                              has_recent_irrigation_history = FALSE)
  e3 <- assess_evidence_level(TRUE, TRUE, TRUE, TRUE, TRUE, "measured_theta",
                              flow_quality = "B_VERIFIED_VOLUMETRIC",
                              flow_operational_verified = TRUE,
                              has_turn_schedule = TRUE,
                              community_aggregation = FALSE,
                              subsoil_water_evidence = "measured_theta",
                              has_recent_irrigation_history = TRUE)
  dplyr::bind_rows(
    new_test_row("EVID01", "Field-capacity root-zone assumption remains E2", e2_assumption == "E2", e2_assumption, "E2"),
    new_test_row("EVID02", "Repeatability alone cannot reach E3", e2_context == "E2", e2_context, "E2"),
    new_test_row("EVID03", "Missing recent irrigation history prevents E3", e2_no_history == "E2", e2_no_history, "E2"),
    new_test_row("EVID04", "Measured water, verified flow, verified turn, and recent history can reach E3", e3 == "E3", e3, "E3")
  )
}

run_all_scientific_tests <- function() {
  profiles <- load_crop_profiles_daily()
  soils <- load_soil_profiles()
  tests <- dplyr::bind_rows(
    run_fao56_example36_tests(),
    run_fao56_example37_tests(),
    run_root_expansion_tests(),
    run_flow_assistant_tests(),
    run_balance_invariant_tests(profiles, soils),
    run_weekly_planner_tests(profiles, soils),
    run_evidence_gate_tests()
  )
  write_csv_utf8(tests, file.path("outputs", "results", "offline_scientific_tests.csv"))
  tests
}
