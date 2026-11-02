html_escape_app <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

format_num_app <- function(x, digits = 2) {
  ifelse(is.finite(as.numeric(x)), formatC(as.numeric(x), format = "f", digits = digits, big.mark = ","), "—")
}

format_duration_hm_app <- function(hours, language = "es") {
  if (is.null(hours) || length(hours) == 0L) return("0 min")
  h <- suppressWarnings(as.numeric(hours[[1]]))
  if (!is.finite(h) || h <= 0) return(if (identical(language, "en")) "0 min" else "0 min")
  total_minutes <- as.integer(round(h * 60))
  hh <- total_minutes %/% 60L
  mm <- total_minutes %% 60L
  if (identical(language, "en")) {
    if (hh > 0L && mm > 0L) return(paste0(hh, " h ", mm, " min"))
    if (hh > 0L) return(paste0(hh, " h"))
    return(paste0(mm, " min"))
  }
  if (hh > 0L && mm > 0L) return(paste0(hh, " h ", mm, " min"))
  if (hh > 0L) return(paste0(hh, " h"))
  paste0(mm, " min")
}

weekday_name_app <- function(date, language = "es") {
  d <- as.Date(date)
  idx <- as.integer(format(d, "%u"))
  es <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
  en <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  if (identical(language, "en")) en[[idx]] else es[[idx]]
}

build_daily_action_plan_table <- function(result) {
  daily <- as.data.frame(result$central_daily, stringsAsFactors = FALSE)
  events <- as.data.frame(result$central_events, stringsAsFactors = FALSE)
  if (!nrow(daily)) {
    return(data.frame(
      date = as.Date(character(0)), weekday_es = character(0), weekday_en = character(0),
      action_code = character(0), action_es = character(0), action_en = character(0),
      planned_volume_m3 = numeric(0), planned_liters = numeric(0), planned_duration_hours = numeric(0),
      planned_duration_es = character(0), planned_duration_en = character(0),
      expected_rain_mm = numeric(0), crop_water_demand_mm = numeric(0), stress_alert = logical(0),
      turn_day = logical(0), note_es = character(0), note_en = character(0),
      stringsAsFactors = FALSE
    ))
  }
  daily$date <- as.Date(daily$date)
  if (anyNA(daily$date)) {
    stop("Daily action source contains invalid dates.", call. = FALSE)
  }
  start_date <- as.Date(result$inputs$current_turn_date)
  next_turn_date <- as.Date(result$inputs$next_turn_date)
  if (is.na(start_date) || is.na(next_turn_date) || next_turn_date <= start_date) {
    stop("Daily action horizon is invalid.", call. = FALSE)
  }
  expected_dates <- seq(start_date, next_turn_date - 1, by = "day")
  expected_chr <- as.character(expected_dates)
  observed_chr <- as.character(daily$date)
  duplicated_chr <- unique(observed_chr[duplicated(observed_chr)])
  if (length(duplicated_chr)) {
    stop(
      "Daily action source contains duplicated dates: ",
      paste(duplicated_chr, collapse = ", "),
      call. = FALSE
    )
  }
  missing_chr <- setdiff(expected_chr, observed_chr)
  extra_chr <- setdiff(observed_chr, expected_chr)
  if (length(missing_chr) || length(extra_chr) || nrow(daily) != length(expected_dates)) {
    stop(
      "Daily action source does not exactly cover the current-turn to next-turn-minus-one horizon. ",
      "Missing: ", if (length(missing_chr)) paste(missing_chr, collapse = ", ") else "none",
      "; extra: ", if (length(extra_chr)) paste(extra_chr, collapse = ", ") else "none",
      "; rows: ", nrow(daily), "; expected: ", length(expected_dates),
      call. = FALSE
    )
  }
  daily <- daily[match(expected_chr, observed_chr), , drop = FALSE]
  rownames(daily) <- NULL
  if (nrow(events)) events$date <- as.Date(events$date)
  rows <- lapply(seq_len(nrow(daily)), function(i) {
    z <- daily[i, , drop = FALSE]
    d <- as.Date(z$date[[1]])
    is_turn <- isTRUE(z$allowed_turn_day[[1]])
    stress <- isTRUE(z$stress_flag[[1]]) || isTRUE(z$turn_conflict[[1]])
    event <- if (nrow(events)) events[events$date == d, , drop = FALSE] else events
    required <- if ("required_volume_m3_event" %in% names(z)) as.numeric(z$required_volume_m3_event[[1]]) else 0
    deliverable <- if ("deliverable_volume_m3_event" %in% names(z)) as.numeric(z$deliverable_volume_m3_event[[1]]) else 0
    unmet <- if ("unmet_volume_m3_event" %in% names(z)) as.numeric(z$unmet_volume_m3_event[[1]]) else 0
    duration <- if (nrow(event) && "deliverable_hours" %in% names(event)) as.numeric(event$deliverable_hours[[1]]) else 0
    if (!is.finite(required)) required <- 0
    if (!is.finite(deliverable)) deliverable <- 0
    if (!is.finite(unmet)) unmet <- 0
    if (!is.finite(duration)) duration <- 0
    tol <- 1e-9

    if (is_turn && required > tol && unmet > tol) {
      code <- "IRRIGATE_LIMITED"
      action_es <- "Regar hasta la capacidad del turno"
      action_en <- "Irrigate up to the turn capacity"
      note_es <- paste0(
        "El cultivo requiere ", format_num_app(required), " m³, pero el turno puede entregar ",
        format_num_app(deliverable), " m³. Quedan ", format_num_app(unmet),
        " m³ sin atender; coordine una alternativa con la organización de riego."
      )
      note_en <- paste0(
        "The crop requires ", format_num_app(required), " m³, but the turn can deliver ",
        format_num_app(deliverable), " m³. A remaining ", format_num_app(unmet),
        " m³ cannot be delivered; coordinate an alternative with the irrigation organisation."
      )
    } else if (is_turn && required > tol) {
      code <- "IRRIGATE"
      action_es <- "Regar"
      action_en <- "Irrigate"
      note_es <- paste0(
        "Aplique ", format_num_app(deliverable), " m³ (", format_num_app(deliverable * 1000, 0),
        " L) durante ", format_duration_hm_app(duration, "es"), "."
      )
      note_en <- paste0(
        "Apply ", format_num_app(deliverable), " m³ (", format_num_app(deliverable * 1000, 0),
        " L) for ", format_duration_hm_app(duration, "en"), "."
      )
    } else if (is_turn) {
      code <- "NO_IRRIGATION_TURN"
      action_es <- "No regar en este turno"
      action_en <- "Do not irrigate at this turn"
      note_es <- "El balance central no requiere aplicar agua en este turno."
      note_en <- "The central balance does not require water application at this turn."
    } else if (stress) {
      code <- "MONITOR_ALERT"
      action_es <- "Vigilar: posible estrés antes del próximo turno"
      action_en <- "Monitor: possible stress before the next turn"
      note_es <- "No existe un turno programado para este día. Revise el estado del cultivo y coordine con la organización si el estrés se confirma."
      note_en <- "No turn is scheduled for this day. Check crop condition and coordinate with the organisation if stress is confirmed."
    } else {
      code <- "NO_SCHEDULED_IRRIGATION"
      action_es <- "No aplicar riego"
      action_en <- "Do not apply irrigation"
      note_es <- "No hay riego programado fuera del turno; continúe la observación del cultivo."
      note_en <- "No irrigation is scheduled outside the turn; continue observing the crop."
    }

    data.frame(
      date = d,
      weekday_es = weekday_name_app(d, "es"),
      weekday_en = weekday_name_app(d, "en"),
      action_code = code,
      action_es = action_es,
      action_en = action_en,
      planned_volume_m3 = if (is_turn) deliverable else 0,
      planned_liters = if (is_turn) deliverable * 1000 else 0,
      planned_duration_hours = if (is_turn) duration else 0,
      planned_duration_es = format_duration_hm_app(if (is_turn) duration else 0, "es"),
      planned_duration_en = format_duration_hm_app(if (is_turn) duration else 0, "en"),
      expected_rain_mm = as.numeric(z$precipitation_mm_day[[1]]),
      crop_water_demand_mm = as.numeric(z$etc_potential_mm_day[[1]]),
      stress_alert = stress,
      turn_day = is_turn,
      note_es = note_es,
      note_en = note_en,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

html_table_app <- function(df, max_rows = 200L) {
  x <- as.data.frame(df, stringsAsFactors = FALSE)
  if (nrow(x) > max_rows) x <- x[seq_len(max_rows), , drop = FALSE]
  if (!nrow(x)) return("<p><em>No rows.</em></p>")
  head <- paste0("<tr>", paste0("<th>", html_escape_app(names(x)), "</th>", collapse = ""), "</tr>")
  body <- apply(x, 1, function(row) {
    paste0("<tr>", paste0("<td>", html_escape_app(row), "</td>", collapse = ""), "</tr>")
  })
  paste0("<table>", head, paste(body, collapse = "\n"), "</table>")
}

simple_report_text <- function(result, language = "es") {
  csum <- result$central_summary[1, , drop = FALSE]
  env <- result$envelope[1, , drop = FALSE]
  labels <- if (identical(language, "en")) {
    list(
      title = "RiegoAzuay DSS — scenario report",
      status = "Development integration build; not the frozen citable release",
      artifact = "Software version",
      created = "Created (UTC)",
      system = "Irrigation system",
      location = "Productive location",
      crop = "Crop and stage",
      unit = "Decision unit",
      area = "Crop-specific area used",
      area_source = "Area source",
      declared_area = "Declared crop area",
      polygon_area = "Polygon-derived crop area",
      required = "Central required volume",
      range = "Combined weather/soil range",
      time = "Required application time",
      capacity = "Turn capacity",
      deliverable = "Deliverable volume",
      unmet = "Unmet volume",
      readiness = "Evidence readiness",
      warnings = "Warnings and boundaries",
      provenance = "Input provenance",
      scenarios = "Combined scenarios",
      daily_plan = "Daily action calendar",
      methods = "Interpretive boundary",
      boundary = "This is a scientific research prototype. The plan applies to one parcel or homogeneous zone planted with the selected crop; it is not the total demand of the irrigation system. It does not operate infrastructure, modify water rights, or constitute field-validated advice. E0–E4 is an internal engineering rule, not an externally validated scale."
    )
  } else {
    list(
      title = "RiegoAzuay DSS — reporte de escenario",
      status = "Compilación de integración en desarrollo; no es el release citable congelado",
      artifact = "Versión del software",
      created = "Creado (UTC)",
      system = "Sistema de riego",
      location = "Ubicación productiva",
      crop = "Cultivo y etapa",
      unit = "Unidad de decisión",
      area = "Área del cultivo utilizada",
      area_source = "Fuente del área",
      declared_area = "Área declarada del cultivo",
      polygon_area = "Área calculada del polígono",
      required = "Volumen central requerido",
      range = "Rango combinado meteorología/suelo",
      time = "Tiempo requerido de aplicación",
      capacity = "Capacidad del turno",
      deliverable = "Volumen entregable",
      unmet = "Volumen no atendido",
      readiness = "Respaldo de los datos",
      warnings = "Advertencias y límites",
      provenance = "Procedencia de insumos",
      scenarios = "Escenarios combinados",
      daily_plan = "Calendario diario de acciones",
      methods = "Límite interpretativo",
      boundary = "Este es un prototipo científico de investigación. El plan corresponde a una parcela o zona homogénea sembrada con el cultivo seleccionado; no es la demanda total del sistema de riego. No opera infraestructura, no modifica derechos de agua ni constituye asesoramiento validado en campo. E0–E4 es una regla interna de ingeniería, no una escala validada externamente."
    )
  }
  stage <- if (identical(language, "en")) result$crop_stage_en else result$crop_stage_es
  list(labels = labels, values = list(
    system = paste0(result$registry_row$public_id[[1]], " — ", result$registry_row$system_name[[1]]),
    location = paste0(result$inputs$productive_latitude, ", ", result$inputs$productive_longitude,
                      " (", result$inputs$productive_location_source, ")"),
    crop = paste0(if (identical(language, "en")) result$profile$crop_name_en[[1]] else result$profile$crop_name_es[[1]],
                  "; ", stage, "; day ", result$crop_age_days),
    unit = if (identical(language, "en")) "One parcel or homogeneous crop zone" else "Una parcela o zona homogénea del cultivo",
    area = paste0(format_num_app(result$inputs$area_ha, 4), " ha"),
    area_source = if (identical(result$inputs$area_source, "polygon")) {
      if (identical(language, "en")) "Polygon-derived area selected by the user" else "Área del polígono seleccionada por el usuario"
    } else {
      if (identical(language, "en")) "Crop area declared by the user" else "Área del cultivo declarada por el usuario"
    },
    declared_area = paste0(format_num_app(result$inputs$declared_area_ha, 4), " ha"),
    polygon_area = if (is.finite(suppressWarnings(as.numeric(result$inputs$polygon_area_ha)))) paste0(format_num_app(result$inputs$polygon_area_ha, 4), " ha") else "—",
    required = paste0(format_num_app(csum$required_volume_m3[[1]]), " m³"),
    range = paste0(format_num_app(env$minimum_required_volume_m3[[1]]), "–",
                   format_num_app(env$maximum_required_volume_m3[[1]]), " m³"),
    time = paste0(format_num_app(csum$required_hours[[1]]), " h"),
    capacity = paste0(format_num_app(env$central_turn_capacity_m3[[1]]), " m³"),
    deliverable = paste0(format_num_app(csum$deliverable_volume_m3[[1]]), " m³"),
    unmet = paste0(format_num_app(csum$unmet_volume_m3[[1]]), " m³"),
    readiness = result$readiness$level
  ))
}

write_bilingual_html_report <- function(result, path) {
  es <- simple_report_text(result, "es")
  en <- simple_report_text(result, "en")
  metric_block <- function(obj) {
    l <- obj$labels; v <- obj$values
    rows <- c(
      paste0("<div><strong>", l$system, ":</strong> ", html_escape_app(v$system), "</div>"),
      paste0("<div><strong>", l$location, ":</strong> ", html_escape_app(v$location), "</div>"),
      paste0("<div><strong>", l$crop, ":</strong> ", html_escape_app(v$crop), "</div>"),
      paste0("<div><strong>", l$unit, ":</strong> ", html_escape_app(v$unit), "</div>"),
      paste0("<div><strong>", l$area, ":</strong> ", html_escape_app(v$area), "</div>"),
      paste0("<div><strong>", l$area_source, ":</strong> ", html_escape_app(v$area_source), "</div>"),
      paste0("<div><strong>", l$declared_area, ":</strong> ", html_escape_app(v$declared_area), "</div>"),
      paste0("<div><strong>", l$polygon_area, ":</strong> ", html_escape_app(v$polygon_area), "</div>"),
      paste0("<div><strong>", l$required, ":</strong> ", v$required, "</div>"),
      paste0("<div><strong>", l$range, ":</strong> ", v$range, "</div>"),
      paste0("<div><strong>", l$time, ":</strong> ", v$time, "</div>"),
      paste0("<div><strong>", l$capacity, ":</strong> ", v$capacity, "</div>"),
      paste0("<div><strong>", l$deliverable, ":</strong> ", v$deliverable, "</div>"),
      paste0("<div><strong>", l$unmet, ":</strong> ", v$unmet, "</div>"),
      paste0("<div><strong>", l$readiness, ":</strong> ", html_escape_app(v$readiness), "</div>")
    )
    paste(rows, collapse = "\n")
  }
  warning_list <- function(language) {
    messages <- if (identical(language, "en")) result$warnings_en else result$warnings_es
    if (!length(messages)) return("<p>—</p>")
    paste0("<ul>", paste0("<li>", html_escape_app(messages), "</li>", collapse = ""), "</ul>")
  }
  daily_plan <- build_daily_action_plan_table(result)
  daily_es <- daily_plan[, c("date", "weekday_es", "action_es", "planned_volume_m3", "planned_liters", "planned_duration_es", "note_es"), drop = FALSE]
  names(daily_es) <- c("Fecha", "Día", "Acción", "Volumen_m3", "Litros", "Duración", "Nota")
  daily_en <- daily_plan[, c("date", "weekday_en", "action_en", "planned_volume_m3", "planned_liters", "planned_duration_en", "note_en"), drop = FALSE]
  names(daily_en) <- c("Date", "Day", "Action", "Volume_m3", "Litres", "Duration", "Note")

  scenario_view <- result$scenario_summary
  keep <- intersect(
    c("soil_scenario", "weather_scenario", "required_volume_m3", "deliverable_volume_m3",
      "unmet_volume_m3", "required_hours", "hours_available", "any_turn_conflict",
      "any_capacity_limit", "weather_covers_to_next_turn"),
    names(scenario_view)
  )
  scenario_view <- scenario_view[, keep, drop = FALSE]
  html <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>RiegoAzuay DSS</title>",
    "<style>body{font-family:Arial,sans-serif;max-width:1100px;margin:30px auto;color:#1f2937;line-height:1.45}",
    "h1,h2{color:#17365d}.banner{background:#e8f1f5;border-left:6px solid #1f6e78;padding:14px;margin-bottom:20px}",
    ".metrics{display:grid;grid-template-columns:1fr 1fr;gap:20px}.panel{border:1px solid #d7dee7;border-radius:10px;padding:18px}",
    "table{border-collapse:collapse;width:100%;font-size:12px}th,td{border:1px solid #d7dee7;padding:6px;text-align:left}th{background:#edf3f7}",
    ".warning{background:#fff7d6;border-left:5px solid #d4a017;padding:12px}.boundary{background:#f7f7f7;padding:12px;border-radius:8px}",
    "@media(max-width:800px){.metrics{grid-template-columns:1fr}}</style></head><body>",
    "<h1>RiegoAzuay DSS</h1><div class='banner'><strong>", html_escape_app(es$labels$status),
    "</strong><br>", html_escape_app(result$software_version), " | ", html_escape_app(result$created_utc), "</div>",
    "<div class='metrics'><section class='panel'><h2>Español</h2>", metric_block(es),
    "<h3>", es$labels$warnings, "</h3><div class='warning'>", warning_list("es"), "</div>",
    "<h3>", es$labels$methods, "</h3><div class='boundary'>", es$labels$boundary, "</div></section>",
    "<section class='panel'><h2>English</h2>", metric_block(en),
    "<h3>", en$labels$warnings, "</h3><div class='warning'>", warning_list("en"), "</div>",
    "<h3>", en$labels$methods, "</h3><div class='boundary'>", en$labels$boundary, "</div></section></div>",
    "<h2>", es$labels$daily_plan, "</h2>", html_table_app(daily_es),
    "<h2>", en$labels$daily_plan, "</h2>", html_table_app(daily_en),
    "<h2>", es$labels$scenarios, " / ", en$labels$scenarios, "</h2>", html_table_app(scenario_view),
    "<h2>", es$labels$provenance, " / ", en$labels$provenance, "</h2>", html_table_app(result$provenance),
    "</body></html>"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(html, path, useBytes = TRUE)
  invisible(path)
}

write_result_csv_export <- function(result, path) {
  x <- result$scenario_summary
  x$run_id <- result$run_id
  x$software_version <- result$software_version
  x$system_public_id <- result$registry_row$public_id[[1]]
  x$crop_profile_id <- result$profile$crop_profile_id[[1]]
  x$crop_age_days <- result$crop_age_days
  x$decision_unit <- "single_crop_homogeneous_zone"
  x$effective_crop_area_ha <- as.numeric(result$inputs$area_ha)
  x$area_source <- as.character(result$inputs$area_source)
  x$declared_crop_area_ha <- as.numeric(result$inputs$declared_area_ha)
  x$polygon_area_ha <- suppressWarnings(as.numeric(result$inputs$polygon_area_ha))
  x$productive_location_source <- as.character(result$inputs$productive_location_source)
  x$productive_latitude <- as.numeric(result$inputs$productive_latitude)
  x$productive_longitude <- as.numeric(result$inputs$productive_longitude)
  x$readiness_level <- result$readiness$level
  write_csv_utf8(x, path)
}

provenance_label_export <- function(class_id, language = "es") {
  es <- c(
    LOCAL_VERIFIED = "Dato local verificado",
    USER_DECLARED = "Declarado por el usuario",
    INSTITUTIONAL_REFERENCE = "Referencia institucional",
    PUBLISHED_REFERENCE = "Parámetro publicado",
    PUBLIC_MODELLED = "Referencia pública o modelada",
    SYNTHETIC_TEST = "Dato de demostración",
    MISSING = "Información faltante"
  )
  en <- c(
    LOCAL_VERIFIED = "Verified local data",
    USER_DECLARED = "User-declared",
    INSTITUTIONAL_REFERENCE = "Institutional reference",
    PUBLISHED_REFERENCE = "Published parameter",
    PUBLIC_MODELLED = "Public or modelled reference",
    SYNTHETIC_TEST = "Demonstration data",
    MISSING = "Missing information"
  )
  labels <- if (identical(language, "en")) en else es
  out <- unname(labels[as.character(class_id)])
  out[is.na(out)] <- as.character(class_id)[is.na(out)]
  out
}

build_user_summary_table <- function(result, language = "es") {
  csum <- result$central_summary[1, , drop = FALSE]
  stage <- if (identical(language, "en")) result$crop_stage_en else result$crop_stage_es
  crop <- if (identical(language, "en")) result$profile$crop_name_en[[1]] else result$profile$crop_name_es[[1]]
  polygon_area <- suppressWarnings(as.numeric(result$inputs$polygon_area_ha))
  area_source <- if (identical(result$inputs$area_source, "polygon")) {
    if (identical(language, "en")) "Polygon-derived area explicitly selected by the user" else "Área calculada del polígono elegida expresamente por el usuario"
  } else {
    if (identical(language, "en")) "Crop area declared by the user" else "Área del cultivo declarada por el usuario"
  }
  field <- if (identical(language, "en")) {
    c(
      "Plan status", "Decision unit", "Irrigation system", "Public application ID", "Canton", "Parish",
      "Representative latitude", "Representative longitude", "Location source", "Crop", "Crop stage",
      "Crop-specific area used (ha)", "Area source", "Declared crop area (ha)", "Polygon-derived crop area (ha)",
      "Turn date", "Next turn date", "Required volume (m³)", "Equivalent litres",
      "Required application time", "Available turn duration (h)", "Deliverable volume (m³)",
      "Unmet volume (m³)", "Internal evidence code", "Interpretive boundary"
    )
  } else {
    c(
      "Estado del plan", "Unidad de decisión", "Sistema de riego", "ID público de aplicación", "Cantón", "Parroquia",
      "Latitud representativa", "Longitud representativa", "Fuente de la ubicación", "Cultivo", "Etapa del cultivo",
      "Área del cultivo utilizada (ha)", "Fuente del área", "Área declarada del cultivo (ha)", "Área del polígono (ha)",
      "Fecha del turno", "Fecha del siguiente turno", "Volumen requerido (m³)", "Litros equivalentes",
      "Tiempo requerido de aplicación", "Duración disponible del turno (h)", "Volumen entregable (m³)",
      "Volumen no atendido (m³)", "Código interno de evidencia", "Límite interpretativo"
    )
  }
  boundary <- if (identical(language, "en")) {
    "Scientific research prototype. The plan applies to one parcel or homogeneous zone planted with the selected crop; it is not the total demand of the irrigation system and is not a field-validated prescription."
  } else {
    "Prototipo científico de investigación. El plan corresponde a una parcela o zona homogénea sembrada con el cultivo seleccionado; no es la demanda total del sistema de riego ni una prescripción validada en campo."
  }
  value <- c(
    if (identical(result$readiness$level, "E2")) {
      if (identical(language, "en")) "Provisional plan" else "Plan provisional"
    } else result$readiness$level,
    if (identical(language, "en")) "One crop-specific parcel or homogeneous zone" else "Una parcela o zona homogénea de un solo cultivo",
    result$registry_row$system_name[[1]],
    result$registry_row$public_id[[1]],
    result$registry_row$canton_display[[1]],
    result$registry_row$parish_display[[1]],
    as.character(result$inputs$productive_latitude),
    as.character(result$inputs$productive_longitude),
    user_location_source_label_app(result$inputs$productive_location_source, language),
    crop,
    stage,
    format_num_app(result$inputs$area_ha, 4),
    area_source,
    format_num_app(result$inputs$declared_area_ha, 4),
    if (is.finite(polygon_area)) format_num_app(polygon_area, 4) else "—",
    as.character(as.Date(result$inputs$current_turn_date)),
    as.character(as.Date(result$inputs$next_turn_date)),
    format_num_app(csum$required_volume_m3[[1]], 2),
    format_num_app(csum$required_liters[[1]], 0),
    format_duration_hm_app(csum$required_hours[[1]], language),
    format_num_app(csum$hours_available[[1]], 2),
    format_num_app(csum$deliverable_volume_m3[[1]], 2),
    format_num_app(csum$unmet_volume_m3[[1]], 2),
    result$readiness$level,
    boundary
  )
  data.frame(field = field, value = value, stringsAsFactors = FALSE)
}

build_user_daily_table <- function(result, language = "es") {
  x <- build_daily_action_plan_table(result)
  if (identical(language, "en")) {
    out <- x[, c(
      "date", "weekday_en", "action_en", "planned_volume_m3", "planned_liters",
      "planned_duration_en", "expected_rain_mm", "crop_water_demand_mm",
      "stress_alert", "note_en"
    ), drop = FALSE]
    names(out) <- c(
      "Date", "Day", "Action", "Volume (m³)", "Litres", "Duration",
      "Expected rain (mm)", "Crop water demand (mm)", "Stress alert", "Note"
    )
  } else {
    out <- x[, c(
      "date", "weekday_es", "action_es", "planned_volume_m3", "planned_liters",
      "planned_duration_es", "expected_rain_mm", "crop_water_demand_mm",
      "stress_alert", "note_es"
    ), drop = FALSE]
    names(out) <- c(
      "Fecha", "Día", "Acción", "Volumen (m³)", "Litros", "Duración",
      "Lluvia esperada (mm)", "Demanda hídrica del cultivo (mm)", "Alerta de estrés", "Nota"
    )
  }
  out[[1]] <- as.Date(out[[1]])
  out[[4]] <- round(as.numeric(out[[4]]), 3)
  out[[5]] <- round(as.numeric(out[[5]]), 0)
  out[[7]] <- round(as.numeric(out[[7]]), 2)
  out[[8]] <- round(as.numeric(out[[8]]), 2)
  out
}

build_user_warning_table <- function(result, language = "es") {
  messages <- if (identical(language, "en")) result$warnings_en else result$warnings_es
  heading <- if (identical(language, "en")) "Warning or boundary" else "Advertencia o límite"
  data.frame(setNames(list(as.character(messages)), heading), check.names = FALSE, stringsAsFactors = FALSE)
}


format_coordinate_user_app <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  if (!is.finite(value)) return("—")
  formatC(value, format = "f", digits = 6)
}

user_location_source_label_app <- function(source, language = "es") {
  source <- safe_character(source)
  es <- c(
    map_click = "Punto marcado por el usuario",
    manual = "Coordenada ingresada por el usuario",
    polygon_centroid = "Centroide del polígono dibujado",
    intake_approximation = "Captación institucional usada como aproximación exploratoria"
  )
  en <- c(
    map_click = "Point marked by the user",
    manual = "User-entered coordinate",
    polygon_centroid = "Centroid of the drawn polygon",
    intake_approximation = "Institutional intake used as an exploratory approximation"
  )
  labels <- if (identical(language, "en")) en else es
  out <- unname(labels[source])
  if (length(out) != 1L || is.na(out) || !nzchar(out)) {
    out <- if (identical(language, "en")) "User-specified representative location" else "Ubicación representativa indicada por el usuario"
  }
  out
}

user_area_source_label_app <- function(source, language = "es") {
  if (identical(safe_character(source), "polygon")) {
    if (identical(language, "en")) "Polygon-derived crop area explicitly selected by the user" else "Área cartográfica del cultivo derivada del polígono y elegida por el usuario"
  } else {
    if (identical(language, "en")) "Crop area declared by the user" else "Área sembrada con el cultivo declarada por el usuario"
  }
}

user_weather_source_label_app <- function(mode, language = "es") {
  mode <- safe_character(mode)
  es <- c(
    offline_fixture = "Datos meteorológicos sintéticos de demostración (sin internet)",
    archived_fixture = "Pronóstico ECMWF guardado para demostración (no es actual)",
    manual_csv = "Archivo meteorológico suministrado por el usuario o una estación local",
    live_openmeteo = "Pronóstico ECMWF IFS en línea mediante Open-Meteo"
  )
  en <- c(
    offline_fixture = "Synthetic demonstration weather data (offline)",
    archived_fixture = "Saved ECMWF demonstration forecast (not current)",
    manual_csv = "Weather file supplied by the user or a local station",
    live_openmeteo = "Live ECMWF IFS forecast via Open-Meteo"
  )
  labels <- if (identical(language, "en")) en else es
  out <- unname(labels[mode])
  if (length(out) != 1L || is.na(out) || !nzchar(out)) {
    out <- if (identical(language, "en")) "Weather source recorded in the technical export" else "Fuente meteorológica registrada en la exportación técnica"
  }
  out
}

user_soil_source_label_app <- function(result, language = "es") {
  mode <- safe_character(result$inputs$soil_mode)
  verified <- isTRUE(result$inputs$local_soil_verified)
  if (identical(language, "en")) {
    labels <- c(
      archived_santul = "Saved SoilGrids 2.0 public reference for the Santul demonstration point",
      fao_reference = "General FAO reference soil",
      live_soilgrids = "SoilGrids 2.0 public reference queried for the parcel location"
    )
    if (identical(mode, "local_uniform")) return(if (verified) "Verified/documented local soil profile" else "User-declared local soil profile")
  } else {
    labels <- c(
      archived_santul = "Referencia pública SoilGrids 2.0 guardada para el punto de demostración de Santul",
      fao_reference = "Suelo general de referencia FAO",
      live_soilgrids = "Referencia pública SoilGrids 2.0 consultada para la ubicación de la parcela"
    )
    if (identical(mode, "local_uniform")) return(if (verified) "Perfil local de suelo verificado o documentado" else "Perfil local de suelo declarado por el usuario")
  }
  out <- unname(labels[mode])
  if (length(out) != 1L || is.na(out) || !nzchar(out)) {
    out <- if (identical(language, "en")) "Soil source recorded in the technical export" else "Fuente del suelo registrada en la exportación técnica"
  }
  out
}

user_flow_source_label_app <- function(source, language = "es") {
  source <- safe_character(source)
  es <- c(
    declared = "Caudal declarado por el usuario",
    institutional = "Caudal institucional o autorizado usado como referencia",
    meter = "Caudal obtenido con medidor instalado validado",
    volumetric = "Caudal obtenido mediante aforo volumétrico documentado",
    calibrated_structure = "Caudal obtenido mediante estructura hidráulica calibrada",
    other_documented = "Caudal obtenido mediante otro método documentado"
  )
  en <- c(
    declared = "Flow declared by the user",
    institutional = "Institutional or authorised flow used as a reference",
    meter = "Flow obtained from a validated installed meter",
    volumetric = "Flow obtained from a documented volumetric measurement",
    calibrated_structure = "Flow obtained from a calibrated hydraulic structure",
    other_documented = "Flow obtained through another documented method"
  )
  labels <- if (identical(language, "en")) en else es
  out <- unname(labels[source])
  if (length(out) != 1L || is.na(out) || !nzchar(out)) {
    out <- if (identical(language, "en")) "Flow source recorded in the technical export" else "Fuente del caudal registrada en la exportación técnica"
  }
  out
}

format_user_turn_schedule_app <- function(result, language = "es") {
  current <- as.Date(result$inputs$current_turn_date)
  next_turn <- as.Date(result$inputs$next_turn_date)
  hours <- format_num_app(result$inputs$turn_hours, 2)
  verified <- isTRUE(result$inputs$turn_schedule_verified)
  if (identical(language, "en")) {
    status <- if (verified) "verified with the irrigation organisation" else "declared by the user; not verified with the irrigation organisation"
    paste0(format(current, "%Y-%m-%d"), " → ", format(next_turn, "%Y-%m-%d"), "; available duration: ", hours, " h; ", status)
  } else {
    status <- if (verified) "verificado con la organización de riego" else "declarado por el usuario; no verificado con la organización de riego"
    paste0(format(current, "%d/%m/%Y"), " → ", format(next_turn, "%d/%m/%Y"), "; duración disponible: ", hours, " h; ", status)
  }
}

format_user_source_value_app <- function(result, input_group, language = "es") {
  input_group <- safe_character(input_group)
  if (identical(input_group, "productive_location")) {
    return(paste0(
      user_location_source_label_app(result$inputs$productive_location_source, language), ": ",
      if (identical(language, "en")) "latitude " else "latitud ",
      format_coordinate_user_app(result$inputs$productive_latitude), "; ",
      if (identical(language, "en")) "longitude " else "longitud ",
      format_coordinate_user_app(result$inputs$productive_longitude)
    ))
  }
  if (identical(input_group, "productive_area")) {
    return(paste0(format_num_app(result$inputs$area_ha, 4), " ha; ", user_area_source_label_app(result$inputs$area_source, language)))
  }
  if (identical(input_group, "weather")) return(user_weather_source_label_app(result$inputs$weather_mode, language))
  if (identical(input_group, "soil")) return(user_soil_source_label_app(result, language))
  if (identical(input_group, "active_flow")) {
    return(paste0(format_num_app(result$inputs$flow_lps, 2), " L/s; ", user_flow_source_label_app(result$inputs$flow_source, language)))
  }
  if (identical(input_group, "turn_schedule")) return(format_user_turn_schedule_app(result, language))
  if (identical(language, "en")) "See technical provenance export" else "Consulte la exportación técnica de procedencia"
}

build_user_source_table <- function(result, language = "es") {
  x <- as.data.frame(result$provenance, stringsAsFactors = FALSE)
  core_ids <- c("productive_location", "productive_area", "weather", "soil", "active_flow", "turn_schedule")
  labels_es <- c(
    productive_location = "Ubicación del cultivo", productive_area = "Área del cultivo", weather = "Meteorología", soil = "Suelo",
    active_flow = "Caudal", turn_schedule = "Calendario de turnos"
  )
  labels_en <- c(
    productive_location = "Crop location", productive_area = "Crop area", weather = "Weather", soil = "Soil",
    active_flow = "Flow", turn_schedule = "Turn schedule"
  )
  labels <- if (identical(language, "en")) labels_en else labels_es
  provenance_index <- match(core_ids, as.character(x$input_group))
  if (anyNA(provenance_index)) {
    missing_ids <- core_ids[is.na(provenance_index)]
    stop("User source export is missing required provenance groups: ", paste(missing_ids, collapse = ", "), call. = FALSE)
  }
  provenance_classes <- as.character(x$provenance_class[provenance_index])
  out <- data.frame(
    item = unname(labels[core_ids]),
    active_value = vapply(core_ids, function(id) format_user_source_value_app(result, id, language), character(1)),
    source_type = provenance_label_export(provenance_classes, language),
    stringsAsFactors = FALSE
  )
  if (anyNA(out) || any(!nzchar(trimws(as.character(out$item)))) || any(!nzchar(trimws(as.character(out$active_value))))) {
    stop("User source export contains an empty or missing display value.", call. = FALSE)
  }
  names(out) <- if (identical(language, "en")) {
    c("Item", "Active value", "Source type")
  } else {
    c("Elemento", "Valor activo", "Tipo de fuente")
  }
  out
}

write_user_xlsx_export <- function(result, path, language = "es") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The openxlsx package is required to create the formatted user Excel export.", call. = FALSE)
  }
  language <- if (identical(language, "en")) "en" else "es"
  sheet_names <- if (identical(language, "en")) {
    c(summary = "Parcel summary", plan = "Daily plan", warnings = "Warnings", sources = "Sources")
  } else {
    c(summary = "Resumen parcela", plan = "Plan diario", warnings = "Advertencias", sources = "Fuentes")
  }
  sheets <- list(
    build_user_summary_table(result, language),
    build_user_daily_table(result, language),
    build_user_warning_table(result, language),
    build_user_source_table(result, language)
  )
  names(sheets) <- unname(sheet_names)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  wb <- openxlsx::createWorkbook(creator = "RiegoAzuay DSS")
  header_style <- openxlsx::createStyle(
    fgFill = "#1F6E78", fontColour = "#FFFFFF", textDecoration = "bold",
    halign = "center", valign = "center", border = "Bottom", borderColour = "#17365D"
  )
  wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  title_style <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#17365D", valign = "top")
  warning_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top", fgFill = "#FFF7D6")
  date_style <- openxlsx::createStyle(numFmt = "yyyy-mm-dd")
  number_style <- openxlsx::createStyle(numFmt = "0.00")

  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm, gridLines = TRUE)
    dat <- sheets[[nm]]
    openxlsx::writeData(wb, nm, dat, headerStyle = header_style, withFilter = FALSE)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
    if (nrow(dat) > 0L) {
      openxlsx::addStyle(wb, nm, wrap_style, rows = 2:(nrow(dat) + 1L), cols = seq_len(ncol(dat)), gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::setRowHeights(wb, nm, rows = 1, heights = 26)
  }

  summary_name <- unname(sheet_names[["summary"]])
  plan_name <- unname(sheet_names[["plan"]])
  warnings_name <- unname(sheet_names[["warnings"]])
  sources_name <- unname(sheet_names[["sources"]])

  openxlsx::setColWidths(wb, summary_name, cols = 1, widths = 34)
  openxlsx::setColWidths(wb, summary_name, cols = 2, widths = 82)
  if (nrow(sheets[[summary_name]]) > 0L) {
    openxlsx::addStyle(wb, summary_name, title_style, rows = 2:(nrow(sheets[[summary_name]]) + 1L), cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(wb, summary_name, rows = 2:(nrow(sheets[[summary_name]]) + 1L), heights = 32)
    openxlsx::setRowHeights(wb, summary_name, rows = nrow(sheets[[summary_name]]) + 1L, heights = 78)
  }

  plan_widths <- pmin(pmax(vapply(sheets[[plan_name]], function(z) max(nchar(as.character(z)), na.rm = TRUE), numeric(1)) + 3, 12), 48)
  plan_widths[!is.finite(plan_widths)] <- 14
  openxlsx::setColWidths(wb, plan_name, cols = seq_along(plan_widths), widths = plan_widths)
  if (nrow(sheets[[plan_name]]) > 0L) {
    openxlsx::addStyle(wb, plan_name, date_style, rows = 2:(nrow(sheets[[plan_name]]) + 1L), cols = 1, gridExpand = TRUE, stack = TRUE)
    numeric_cols <- which(vapply(sheets[[plan_name]], is.numeric, logical(1)))
    if (length(numeric_cols)) {
      openxlsx::addStyle(wb, plan_name, number_style, rows = 2:(nrow(sheets[[plan_name]]) + 1L), cols = numeric_cols, gridExpand = TRUE, stack = TRUE)
    }
  }

  openxlsx::setColWidths(wb, warnings_name, cols = 1, widths = 115)
  if (nrow(sheets[[warnings_name]]) > 0L) {
    openxlsx::addStyle(wb, warnings_name, warning_style, rows = 2:(nrow(sheets[[warnings_name]]) + 1L), cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::setRowHeights(wb, warnings_name, rows = 2:(nrow(sheets[[warnings_name]]) + 1L), heights = 58)
  }

  source_widths <- c(26, 72, 34)
  openxlsx::setColWidths(wb, sources_name, cols = seq_along(source_widths), widths = source_widths)
  openxlsx::setRowHeights(wb, sources_name, rows = 2:(nrow(sheets[[sources_name]]) + 1L), heights = 42)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  if (!file.exists(path) || !is.finite(file.info(path)$size) || file.info(path)$size <= 0) {
    stop("The Excel export was not created correctly.", call. = FALSE)
  }
  invisible(path)
}

copy_if_exists_app <- function(source, destination) {
  if (is.null(source) || !length(source) || is.na(source) || !file.exists(source)) return(FALSE)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(source)) {
    files <- list.files(source, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    for (f in files) {
      rel <- substring(f, nchar(source) + 2L)
      target <- file.path(destination, rel)
      if (dir.exists(f)) dir.create(target, recursive = TRUE, showWarnings = FALSE) else {
        dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
        file.copy(f, target, overwrite = TRUE)
      }
    }
  } else {
    file.copy(source, destination, overwrite = TRUE)
  }
  TRUE
}

write_sha256_manifest <- function(root_dir, path = file.path(root_dir, "MANIFEST_SHA256.csv")) {
  root_abs <- normalizePath(root_dir, winslash = "/", mustWork = TRUE)
  path_abs <- normalizePath(path, winslash = "/", mustWork = FALSE)
  files_rel <- list.files(
    root_abs,
    recursive = TRUE,
    full.names = FALSE,
    all.files = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  files_rel <- gsub("\\\\", "/", files_rel)
  files_abs <- file.path(root_abs, files_rel)
  keep <- normalizePath(files_abs, winslash = "/", mustWork = FALSE) != path_abs
  files_rel <- files_rel[keep]
  files_abs <- files_abs[keep]
  manifest <- data.frame(
    relative_path = files_rel,
    size_bytes = unname(file.info(files_abs)$size),
    sha256 = vapply(files_abs, hash_file_sha256, character(1)),
    stringsAsFactors = FALSE
  )
  manifest <- manifest[order(manifest$relative_path), , drop = FALSE]
  if (!nrow(manifest) || anyNA(manifest$relative_path) || any(!nzchar(trimws(manifest$relative_path)))) {
    stop("SHA-256 manifest contains blank relative paths.", call. = FALSE)
  }
  if (anyDuplicated(manifest$relative_path)) {
    stop("SHA-256 manifest contains duplicated relative paths.", call. = FALSE)
  }
  unsafe <- grepl("^([A-Za-z]:|/|\\\\)", manifest$relative_path) |
    vapply(strsplit(manifest$relative_path, "/", fixed = TRUE), function(x) ".." %in% x, logical(1))
  if (any(unsafe)) {
    stop("SHA-256 manifest contains absolute or parent-traversal paths.", call. = FALSE)
  }
  write_csv_utf8(manifest, path)
  manifest
}

verify_sha256_manifest <- function(root_dir, path = file.path(root_dir, "MANIFEST_SHA256.csv"), stop_on_failure = TRUE) {
  root_abs <- normalizePath(root_dir, winslash = "/", mustWork = TRUE)
  path_abs <- normalizePath(path, winslash = "/", mustWork = TRUE)
  manifest <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  required <- c("relative_path", "size_bytes", "sha256")
  schema_ok <- all(required %in% names(manifest))
  if (!schema_ok) {
    result <- data.frame(check = "schema", status = "FAIL", detail = paste(names(manifest), collapse = " | "), stringsAsFactors = FALSE)
    if (isTRUE(stop_on_failure)) stop("SHA-256 manifest schema is invalid.", call. = FALSE)
    return(result)
  }
  rel <- gsub("\\\\", "/", as.character(manifest$relative_path))
  listed_abs <- file.path(root_abs, rel)
  all_rel <- list.files(root_abs, recursive = TRUE, full.names = FALSE, all.files = TRUE, include.dirs = FALSE, no.. = TRUE)
  all_rel <- gsub("\\\\", "/", all_rel)
  manifest_rel <- substring(path_abs, nchar(root_abs) + 2L)
  all_rel <- setdiff(all_rel, manifest_rel)
  safe <- !is.na(rel) & nzchar(trimws(rel)) &
    !grepl("^([A-Za-z]:|/|\\\\)", rel) &
    !vapply(strsplit(rel, "/", fixed = TRUE), function(x) ".." %in% x, logical(1))
  exists <- file.exists(listed_abs)
  actual_size <- rep(NA_real_, length(rel))
  actual_hash <- rep(NA_character_, length(rel))
  if (any(exists)) {
    actual_size[exists] <- unname(file.info(listed_abs[exists])$size)
    actual_hash[exists] <- vapply(listed_abs[exists], hash_file_sha256, character(1))
  }
  expected_size <- suppressWarnings(as.numeric(manifest$size_bytes))
  expected_hash <- tolower(trimws(as.character(manifest$sha256)))
  row_ok <- safe & exists & is.finite(expected_size) & actual_size == expected_size &
    nzchar(expected_hash) & tolower(actual_hash) == expected_hash
  checks <- data.frame(
    check = c("nonblank_safe_paths", "unique_paths", "exact_file_coverage", "all_files_exist", "all_sizes_match", "all_hashes_match"),
    status = c(
      ifelse(all(safe), "PASS", "FAIL"),
      ifelse(!anyDuplicated(rel), "PASS", "FAIL"),
      ifelse(setequal(rel, all_rel) && length(rel) == length(all_rel), "PASS", "FAIL"),
      ifelse(all(exists), "PASS", "FAIL"),
      ifelse(all(exists & is.finite(expected_size) & actual_size == expected_size), "PASS", "FAIL"),
      ifelse(all(exists & nzchar(expected_hash) & tolower(actual_hash) == expected_hash), "PASS", "FAIL")
    ),
    detail = c(
      paste(sum(safe), "/", length(safe)),
      paste(length(unique(rel)), "/", length(rel)),
      paste(length(rel), "listed;", length(all_rel), "actual"),
      paste(sum(exists), "/", length(exists)),
      paste(sum(exists & is.finite(expected_size) & actual_size == expected_size), "/", length(rel)),
      paste(sum(exists & nzchar(expected_hash) & tolower(actual_hash) == expected_hash), "/", length(rel))
    ),
    stringsAsFactors = FALSE
  )
  attr(checks, "row_verification") <- data.frame(
    relative_path = rel,
    exists = exists,
    size_match = exists & is.finite(expected_size) & actual_size == expected_size,
    hash_match = exists & nzchar(expected_hash) & tolower(actual_hash) == expected_hash,
    row_ok = row_ok,
    stringsAsFactors = FALSE
  )
  if (isTRUE(stop_on_failure) && any(checks$status != "PASS")) {
    stop("SHA-256 manifest verification failed: ", paste(checks$check[checks$status != "PASS"], collapse = ", "), call. = FALSE)
  }
  checks
}

write_verified_zip_app <- function(staging_dir, zip_path) {
  if (!dir.exists(staging_dir)) {
    stop("ZIP staging directory not found: ", staging_dir, call. = FALSE)
  }
  staging_abs <- normalizePath(staging_dir, winslash = "/", mustWork = TRUE)
  target_dir <- dirname(zip_path)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  target_dir_abs <- normalizePath(target_dir, winslash = "/", mustWork = TRUE)
  zip_abs <- file.path(target_dir_abs, basename(zip_path))

  files_rel <- list.files(
    staging_abs,
    recursive = TRUE,
    all.files = TRUE,
    full.names = FALSE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  if (!length(files_rel)) {
    stop("ZIP staging directory contains no files: ", staging_abs, call. = FALSE)
  }
  missing_sources <- files_rel[!file.exists(file.path(staging_abs, files_rel))]
  if (length(missing_sources)) {
    stop("ZIP source files are missing: ", paste(missing_sources, collapse = ", "), call. = FALSE)
  }

  temp_zip <- tempfile(
    pattern = paste0(".", tools::file_path_sans_ext(basename(zip_abs)), "_"),
    tmpdir = target_dir_abs,
    fileext = ".zip"
  )
  cleanup_temp <- TRUE
  on.exit({
    if (cleanup_temp && file.exists(temp_zip)) unlink(temp_zip, force = TRUE)
  }, add = TRUE)

  zip::zip(
    zipfile = temp_zip,
    files = files_rel,
    recurse = FALSE,
    include_directories = FALSE,
    root = staging_abs,
    mode = "mirror"
  )
  if (!file.exists(temp_zip) || !is.finite(file.info(temp_zip)$size) || file.info(temp_zip)$size <= 0) {
    stop("Temporary ZIP archive was not created correctly: ", temp_zip, call. = FALSE)
  }

  expected_entries <- gsub("\\\\", "/", files_rel)
  archive_entries <- gsub("\\\\", "/", utils::unzip(temp_zip, list = TRUE)$Name)
  archive_entries <- sub("^\\./", "", archive_entries)
  missing_entries <- setdiff(expected_entries, archive_entries)
  if (length(missing_entries)) {
    stop("ZIP archive is incomplete: ", paste(missing_entries, collapse = ", "), call. = FALSE)
  }

  if (file.exists(zip_abs)) unlink(zip_abs, force = TRUE)
  moved <- file.rename(temp_zip, zip_abs)
  if (!isTRUE(moved)) {
    copied <- file.copy(temp_zip, zip_abs, overwrite = TRUE)
    if (!isTRUE(copied)) {
      stop("Verified ZIP archive could not be moved to: ", zip_abs, call. = FALSE)
    }
    unlink(temp_zip, force = TRUE)
  }
  cleanup_temp <- FALSE
  if (!file.exists(zip_abs) || !is.finite(file.info(zip_abs)$size) || file.info(zip_abs)$size <= 0) {
    stop("Final ZIP archive was not created correctly: ", zip_abs, call. = FALSE)
  }
  normalizePath(zip_abs, winslash = "/", mustWork = TRUE)
}

build_reproducibility_export <- function(result, zip_path) {
  staging <- tempfile(paste0("RiegoAzuay_", result$run_id, "_"))
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(staging, "results"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(staging, "inputs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(staging, "provenance"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(staging, "source_receipts"), recursive = TRUE, showWarnings = FALSE)

  write_csv_utf8(result$scenario_summary, file.path(staging, "results", "scenario_summary.csv"))
  write_csv_utf8(result$central_daily, file.path(staging, "results", "central_daily_balance.csv"))
  write_csv_utf8(build_daily_action_plan_table(result), file.path(staging, "results", "daily_action_plan_ES_EN.csv"))
  write_csv_utf8(result$central_events, file.path(staging, "results", "central_turn_events.csv"))
  write_csv_utf8(result$daily_all, file.path(staging, "results", "all_daily_scenarios.csv"))
  write_csv_utf8(result$events_all, file.path(staging, "results", "all_turn_events.csv"))
  write_csv_utf8(result$diagnostics, file.path(staging, "results", "diagnostics.csv"))
  write_csv_utf8(result$input_snapshot, file.path(staging, "inputs", "input_snapshot.csv"))
  write_csv_utf8(result$registry_row, file.path(staging, "inputs", "selected_system_only.csv"))
  write_csv_utf8(result$profile, file.path(staging, "inputs", "active_crop_profile.csv"))
  write_csv_utf8(result$soil_layers, file.path(staging, "inputs", "active_soil_layers.csv"))
  write_csv_utf8(result$weather, file.path(staging, "inputs", "active_weather_scenarios.csv"))
  write_csv_utf8(result$provenance, file.path(staging, "provenance", "input_provenance.csv"))
  write_csv_utf8(result$warnings_table, file.path(staging, "provenance", "warnings_bilingual.csv"))
  writeLines(result$warnings_es, file.path(staging, "provenance", "warnings_es.txt"), useBytes = TRUE)
  writeLines(result$warnings_en, file.path(staging, "provenance", "warnings_en.txt"), useBytes = TRUE)
  if (!is.null(result$weather_selection)) write_csv_utf8(result$weather_selection, file.path(staging, "provenance", "ensemble_member_selection.csv"))
  if (!is.null(result$weather_distribution)) write_csv_utf8(result$weather_distribution, file.path(staging, "provenance", "ensemble_member_distribution.csv"))
  if (!is.null(result$external_run_dirs$weather)) copy_if_exists_app(result$external_run_dirs$weather, file.path(staging, "source_receipts", "weather"))
  if (!is.null(result$external_run_dirs$soil)) copy_if_exists_app(result$external_run_dirs$soil, file.path(staging, "source_receipts", "soil"))
  write_user_xlsx_export(result, file.path(staging, "results", "RiegoAzuay_user_plan_ES.xlsx"), "es")
  write_user_xlsx_export(result, file.path(staging, "results", "RiegoAzuay_user_plan_EN.xlsx"), "en")
  write_bilingual_html_report(result, file.path(staging, "RiegoAzuay_DSS_report_ES_EN.html"))
  saveRDS(result, file.path(staging, "scenario_result.rds"))
  metadata <- data.frame(
    field = c("software", "version", "run_id", "created_utc", "readiness", "data_governance"),
    value = c("RiegoAzuay DSS", result$software_version, result$run_id, result$created_utc,
              result$readiness$level,
              "The complete restricted institutional registry is not included; only the selected application record is exported."),
    stringsAsFactors = FALSE
  )
  write_csv_utf8(metadata, file.path(staging, "REPRODUCIBILITY_METADATA.csv"))
  write_sha256_manifest(staging)
  zip_abs <- write_verified_zip_app(staging, zip_path)
  invisible(zip_abs)
}
