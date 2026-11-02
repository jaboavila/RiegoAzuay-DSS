attach_depth_columns <- function(x) {
  parts <- lapply(x$depth_interval, parse_depth_interval)
  d <- do.call(rbind, parts)
  cbind(x, d)
}

soil_value_lookup <- function(values, role, depth_interval, statistic) {
  z <- values[values$property_role == role & values$depth_interval == depth_interval & values$statistic == statistic, , drop = FALSE]
  if (nrow(z) != 1L) stop("Soil value missing or duplicated: ", role, " / ", depth_interval, " / ", statistic, call. = FALSE)
  as.numeric(z$converted_value[[1]])
}

texture_value_lookup <- function(values, property_code, depth_interval) {
  z <- values[values$property_role == "texture" & values$property_code_requested == property_code &
                values$depth_interval == depth_interval & values$statistic == "mean", , drop = FALSE]
  if (nrow(z) == 0L || !is.finite(z$converted_value[[1]])) return(NA_real_)
  as.numeric(z$converted_value[[1]])
}

build_soil_layer_scenarios <- function(values, max_root_depth_m) {
  values <- attach_depth_columns(values)
  depth_table <- unique(values[values$depth_top_m < max_root_depth_m + 1e-12,
                               c("depth_interval", "depth_top_m", "depth_bottom_m")])
  depth_table <- depth_table[order(depth_table$depth_top_m), , drop = FALSE]
  out <- list()
  k <- 1L
  scenario_defs <- list(
    low_storage = c(fc = "Q05", wp = "Q05"),
    central = c(fc = "Q50", wp = "Q50"),
    high_storage = c(fc = "Q95", wp = "Q95")
  )
  for (scenario in names(scenario_defs)) {
    def <- scenario_defs[[scenario]]
    for (i in seq_len(nrow(depth_table))) {
      depth <- depth_table$depth_interval[[i]]
      fc <- soil_value_lookup(values, "field_capacity", depth, def[["fc"]])
      wp <- soil_value_lookup(values, "wilting_point", depth, def[["wp"]])
      valid <- is.finite(fc) && is.finite(wp) && fc > wp
      out[[k]] <- data.frame(
        soil_scenario = scenario,
        depth_interval = depth,
        depth_top_m = depth_table$depth_top_m[[i]],
        depth_bottom_m = depth_table$depth_bottom_m[[i]],
        theta_fc = fc,
        theta_wp = wp,
        available_water_fraction = fc - wp,
        field_capacity_quantile = def[["fc"]],
        wilting_point_quantile = def[["wp"]],
        sand_fraction = texture_value_lookup(values, "sand", depth),
        silt_fraction = texture_value_lookup(values, "silt", depth),
        clay_fraction = texture_value_lookup(values, "clay", depth),
        profile_valid = valid,
        source_type = "SOILGRIDS_250M_REFERENCE",
        uncertainty_interpretation = if (scenario == "central") {
          "Matched Q50 field-capacity and Q50 wilting-point sensitivity profile"
        } else if (scenario == "low_storage") {
          "Matched Q05 field-capacity and Q05 wilting-point sensitivity profile; not a joint probabilistic interval"
        } else {
          "Matched Q95 field-capacity and Q95 wilting-point sensitivity profile; not a joint probabilistic interval"
        },
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  z <- do.call(rbind, out)
  validity <- aggregate(profile_valid ~ soil_scenario, data = z, FUN = all)
  pairs <- unique(z[, c("soil_scenario", "field_capacity_quantile", "wilting_point_quantile")])
  validity <- merge(validity, pairs, by = "soil_scenario", all.x = TRUE, sort = FALSE)
  validity$action <- ifelse(validity$profile_valid, "retained", "excluded_invalid_matched_quantile_profile")
  if (!isTRUE(validity$profile_valid[validity$soil_scenario == "central"])) {
    stop("The central SoilGrids profile has theta_fc <= theta_wp in at least one layer.", call. = FALSE)
  }
  valid_scenarios <- validity$soil_scenario[validity$profile_valid]
  z <- z[z$soil_scenario %in% valid_scenarios, , drop = FALSE]
  attr(z, "validity_audit") <- validity
  z
}

read_local_soil_override <- function(path) {
  cfg <- read_key_value_config(path)
  enabled <- parse_bool(cfg$use_local_override, FALSE)
  if (!enabled) return(list(enabled = FALSE, valid = FALSE, reason = "disabled", layers = NULL, config = cfg))
  fc <- suppressWarnings(as.numeric(cfg$theta_fc))
  wp <- suppressWarnings(as.numeric(cfg$theta_wp))
  top_cm <- suppressWarnings(as.numeric(cfg$measurement_depth_top_cm))
  bottom_cm <- suppressWarnings(as.numeric(cfg$measurement_depth_bottom_cm))
  verified <- identical(tolower(trimws(cfg$verification_status %||% "")), "verified")
  date_ok <- !is.na(as.Date(cfg$measurement_date %||% ""))
  method_ok <- text_present(cfg$measurement_method)
  source_ok <- text_present(cfg$source_description)
  valid <- is.finite(fc) && is.finite(wp) && fc > wp && is.finite(top_cm) && is.finite(bottom_cm) && bottom_cm > top_cm && verified && date_ok && method_ok && source_ok
  if (!valid) return(list(enabled = TRUE, valid = FALSE, reason = "incomplete_or_unverified", layers = NULL, config = cfg))
  layers <- data.frame(
    soil_scenario = "local_verified",
    depth_interval = paste0(top_cm, "-", bottom_cm, "cm"),
    depth_top_m = top_cm / 100,
    depth_bottom_m = bottom_cm / 100,
    theta_fc = fc,
    theta_wp = wp,
    available_water_fraction = fc - wp,
    field_capacity_quantile = "LOCAL",
    wilting_point_quantile = "LOCAL",
    sand_fraction = NA_real_, silt_fraction = NA_real_, clay_fraction = NA_real_,
    profile_valid = TRUE,
    source_type = "LOCAL_VERIFIED_OVERRIDE",
    uncertainty_interpretation = "Verified local values override public reference layers",
    stringsAsFactors = FALSE
  )
  list(enabled = TRUE, valid = TRUE, reason = "verified", layers = layers, config = cfg)
}

expand_uniform_local_layers <- function(local_layer, standard_depths, max_root_depth_m) {
  d <- standard_depths[standard_depths$depth_top_m < max_root_depth_m + 1e-12, , drop = FALSE]
  if (max(local_layer$depth_bottom_m) + 1e-12 < max_root_depth_m) stop("Local soil override does not cover crop scheduling root depth.", call. = FALSE)
  data.frame(
    soil_scenario = "local_verified",
    depth_interval = d$depth_interval,
    depth_top_m = d$depth_top_m,
    depth_bottom_m = d$depth_bottom_m,
    theta_fc = local_layer$theta_fc[[1]],
    theta_wp = local_layer$theta_wp[[1]],
    available_water_fraction = local_layer$available_water_fraction[[1]],
    field_capacity_quantile = "LOCAL",
    wilting_point_quantile = "LOCAL",
    sand_fraction = NA_real_, silt_fraction = NA_real_, clay_fraction = NA_real_,
    profile_valid = TRUE,
    source_type = "LOCAL_VERIFIED_OVERRIDE",
    uncertainty_interpretation = "Verified local uniform values applied over the measured depth",
    stringsAsFactors = FALSE
  )
}

summarise_root_zone_profiles <- function(layer_scenarios, root_depth_m, p_table = NA_real_) {
  out <- list()
  scenarios <- unique(layer_scenarios$soil_scenario)
  for (i in seq_along(scenarios)) {
    s <- scenarios[[i]]
    z <- layer_scenarios[layer_scenarios$soil_scenario == s, , drop = FALSE]
    taw <- layered_taw_mm(z, root_depth_m)
    fc <- layered_equivalent_theta(z, root_depth_m, "theta_fc")
    wp <- layered_equivalent_theta(z, root_depth_m, "theta_wp")
    raw <- if (is.finite(p_table)) taw * p_table else NA_real_
    out[[i]] <- data.frame(
      soil_scenario = s, root_depth_m = root_depth_m,
      theta_fc_equivalent = fc, theta_wp_equivalent = wp,
      available_water_mm_per_m_equivalent = (fc - wp) * 1000,
      taw_mm = taw, raw_mm_unadjusted = raw,
      field_capacity_quantile = unique(z$field_capacity_quantile)[[1]],
      wilting_point_quantile = unique(z$wilting_point_quantile)[[1]],
      source_type = unique(z$source_type)[[1]],
      uncertainty_interpretation = unique(z$uncertainty_interpretation)[[1]],
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

rank_member_index <- function(n, probability) {
  as.integer(clamp_value(round(1 + (n - 1) * probability), 1, n))
}

summarise_member_distribution <- function(plan_summary, soil_scenario) {
  z <- plan_summary[plan_summary$soil_scenario == soil_scenario, , drop = FALSE]
  z <- z[order(z$required_volume_m3, z$weather_scenario), , drop = FALSE]
  n <- nrow(z)
  if (n == 0L) stop("No member plans for soil scenario: ", soil_scenario, call. = FALSE)
  i10 <- rank_member_index(n, 0.10)
  i50 <- rank_member_index(n, 0.50)
  i90 <- rank_member_index(n, 0.90)
  positive_share <- mean(z$required_volume_m3 > 1e-9)
  consensus <- if (positive_share <= 0.10 && z$required_volume_m3[[i90]] <= 1e-9) {
    "HIGH_NO_IRRIGATION_CONSENSUS"
  } else if (positive_share >= 0.90 && z$required_volume_m3[[i10]] > 1e-9) {
    "HIGH_IRRIGATION_CONSENSUS"
  } else {
    "MIXED_WEATHER_SIGNAL"
  }
  data.frame(
    soil_scenario = soil_scenario, member_count = n,
    member_rank10 = z$weather_scenario[[i10]], rank10_required_volume_m3 = z$required_volume_m3[[i10]],
    member_rank50 = z$weather_scenario[[i50]], rank50_required_volume_m3 = z$required_volume_m3[[i50]],
    member_rank90 = z$weather_scenario[[i90]], rank90_required_volume_m3 = z$required_volume_m3[[i90]],
    minimum_required_volume_m3 = min(z$required_volume_m3),
    maximum_required_volume_m3 = max(z$required_volume_m3),
    positive_irrigation_member_count = sum(z$required_volume_m3 > 1e-9),
    positive_irrigation_member_share = positive_share,
    weather_consensus_class = consensus,
    stringsAsFactors = FALSE
  )
}
