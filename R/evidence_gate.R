assess_evidence_level <- function(has_crop, has_area, has_stage_or_date, has_soil,
                                  has_weather, initial_water_evidence,
                                  flow_quality = NA_character_,
                                  flow_operational_verified = FALSE,
                                  has_turn_schedule = FALSE,
                                  community_aggregation = FALSE,
                                  subsoil_water_evidence = "unknown",
                                  has_recent_irrigation_history = FALSE) {
  if (!all(c(has_crop, has_area))) return("E0")
  if (!has_stage_or_date || !has_soil || !has_weather) return("E1")
  measured_initial <- initial_water_evidence %in% c(
    "measured_theta", "sensor", "recent_full_wetting_verified"
  )
  measured_flow <- isTRUE(flow_operational_verified)
  defensible_subsoil <- subsoil_water_evidence %in% c(
    "measured_theta", "sensor", "recent_full_wetting_verified", "no_root_expansion"
  )
  if (community_aggregation && measured_flow && has_turn_schedule && has_recent_irrigation_history && measured_initial && defensible_subsoil) return("E4")
  if (measured_initial && measured_flow && has_turn_schedule && has_recent_irrigation_history && defensible_subsoil) return("E3")
  "E2"
}
