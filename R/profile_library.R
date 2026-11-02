load_crop_profiles_daily <- function(path = file.path("config", "crop_profiles_daily_v2.csv")) {
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  num_cols <- c("cycle_days", "initial_days", "development_days", "mid_days", "late_days",
                "kc_initial", "kc_mid", "kc_end", "zr_initial_m", "zr_min_m",
                "zr_max_range_m", "zr_scheduling_m", "p_table")
  x[num_cols] <- lapply(x[num_cols], as.numeric)
  x
}

load_soil_profiles <- function(path = file.path("config", "soil_reference_profiles_v1.csv")) {
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  num_cols <- c("theta_fc", "theta_wp", "available_water_mm_per_m")
  x[num_cols] <- lapply(x[num_cols], as.numeric)
  x
}

get_crop_profile <- function(profiles, crop_profile_id) {
  z <- profiles[profiles$crop_profile_id == crop_profile_id, , drop = FALSE]
  if (nrow(z) != 1L) stop("crop_profile_id not found or duplicated: ", crop_profile_id, call. = FALSE)
  z
}

get_soil_profile <- function(soils, soil_profile_id) {
  z <- soils[soils$soil_profile_id == soil_profile_id, , drop = FALSE]
  if (nrow(z) != 1L) stop("soil_profile_id not found or duplicated: ", soil_profile_id, call. = FALSE)
  z
}

build_kc_curve <- function(profile_row) {
  ini <- rep(profile_row$kc_initial[[1]], as.integer(profile_row$initial_days[[1]]))
  dev_n <- as.integer(profile_row$development_days[[1]])
  dev <- if (dev_n > 0L) {
    seq(profile_row$kc_initial[[1]], profile_row$kc_mid[[1]], length.out = dev_n + 1L)[-1L]
  } else numeric(0)
  mid <- rep(profile_row$kc_mid[[1]], as.integer(profile_row$mid_days[[1]]))
  late_n <- as.integer(profile_row$late_days[[1]])
  late <- if (late_n > 0L) {
    seq(profile_row$kc_mid[[1]], profile_row$kc_end[[1]], length.out = late_n + 1L)[-1L]
  } else numeric(0)
  out <- c(ini, dev, mid, late)
  expected <- as.integer(profile_row$cycle_days[[1]])
  if (length(out) != expected) stop("Kc curve length does not match crop cycle.", call. = FALSE)
  out
}

build_root_depth_curve <- function(profile_row) {
  n <- as.integer(profile_row$cycle_days[[1]])
  z0 <- as.numeric(profile_row$zr_initial_m[[1]])
  zmax <- as.numeric(profile_row$zr_scheduling_m[[1]])
  growth_end <- as.integer(profile_row$initial_days[[1]] + profile_row$development_days[[1]])
  days <- seq_len(n)
  if (growth_end <= 1L) return(rep(zmax, n))
  z <- z0 + (zmax - z0) * (days - 1) / (growth_end - 1)
  z[days >= growth_end] <- zmax
  clamp_value(z, min(z0, zmax), max(z0, zmax))
}

crop_stage_name <- function(profile_row, crop_day, language = "es") {
  d1 <- profile_row$initial_days[[1]]
  d2 <- d1 + profile_row$development_days[[1]]
  d3 <- d2 + profile_row$mid_days[[1]]
  key <- if (crop_day <= d1) "initial" else if (crop_day <= d2) "development" else if (crop_day <= d3) "mid" else "late"
  labels <- list(
    es = c(initial = "inicial", development = "desarrollo", mid = "media o máxima demanda", late = "final"),
    en = c(initial = "initial", development = "development", mid = "mid-season or peak demand", late = "late season")
  )
  labels[[language]][[key]]
}
