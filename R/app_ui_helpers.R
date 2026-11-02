metric_card_ui <- function(label, value, note = NULL, class = "") {
  div(
    class = paste("metric-card", class),
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    if (!is.null(note)) div(class = "metric-note", note)
  )
}

notice_box_ui <- function(text, type = c("info", "warning", "success", "danger")) {
  type <- match.arg(type)
  div(class = paste0("notice-box notice-", type), text)
}

format_metric <- function(x, digits = 2, suffix = "") {
  if (!length(x) || !is.finite(as.numeric(x[[1]]))) return("—")
  paste0(formatC(as.numeric(x[[1]]), format = "f", digits = digits, big.mark = ","), suffix)
}

scenario_label_app <- function(x, language = "es") {
  x <- as.character(x)
  if (identical(language, "en")) {
    unname(c(lower = "Lower-demand", central = "Central", upper = "Upper-demand")[x] %||% x)
  } else {
    unname(c(lower = "Menor demanda", central = "Central", upper = "Mayor demanda")[x] %||% x)
  }
}

readiness_label_app <- function(level, evidence_levels, language = "es") {
  z <- evidence_levels[evidence_levels$level == level, , drop = FALSE]
  if (!nrow(z)) return(level)
  if (identical(language, "en")) z$label_en[[1]] else z$label_es[[1]]
}

provenance_label_app <- function(class_id, provenance_classes, language = "es") {
  z <- provenance_classes[provenance_classes$class_id == class_id, , drop = FALSE]
  if (!nrow(z)) return(class_id)
  if (identical(language, "en")) z$label_en[[1]] else z$label_es[[1]]
}

render_value_tag <- function(value, class = "tag-neutral") {
  span(class = paste("value-tag", class), value)
}

source_class_css <- function(class_id) {
  switch(
    as.character(class_id),
    LOCAL_VERIFIED = "tag-verified",
    PUBLIC_MODELLED = "tag-modelled",
    PUBLISHED_REFERENCE = "tag-reference",
    INSTITUTIONAL_REFERENCE = "tag-institutional",
    USER_DECLARED = "tag-declared",
    SYNTHETIC_TEST = "tag-test",
    MISSING = "tag-missing",
    "tag-neutral"
  )
}


daily_action_class_app <- function(action_code) {
  switch(
    as.character(action_code),
    IRRIGATE = "daily-action-irrigate",
    IRRIGATE_LIMITED = "daily-action-warning",
    MONITOR_ALERT = "daily-action-warning",
    NO_IRRIGATION_TURN = "daily-action-neutral",
    NO_SCHEDULED_IRRIGATION = "daily-action-neutral",
    "daily-action-neutral"
  )
}

daily_action_cards_ui <- function(plan, language = "es") {
  x <- as.data.frame(plan, stringsAsFactors = FALSE)
  if (!nrow(x)) return(NULL)
  action_col <- if (identical(language, "en")) "action_en" else "action_es"
  weekday_col <- if (identical(language, "en")) "weekday_en" else "weekday_es"
  note_col <- if (identical(language, "en")) "note_en" else "note_es"
  cards <- lapply(seq_len(nrow(x)), function(i) {
    z <- x[i, , drop = FALSE]
    amount <- if (z$planned_volume_m3[[1]] > 1e-9) {
      paste0(
        format_metric(z$planned_volume_m3[[1]], 2, " m³"),
        " · ", format_metric(z$planned_liters[[1]], 0, " L"),
        " · ", if (identical(language, "en")) z$planned_duration_en[[1]] else z$planned_duration_es[[1]]
      )
    } else {
      if (identical(language, "en")) "0 m³ planned" else "0 m³ programados"
    }
    div(
      class = paste("daily-plan-card", daily_action_class_app(z$action_code[[1]])),
      div(class = "daily-plan-date", paste0(z[[weekday_col]][[1]], " · ", format(as.Date(z$date[[1]]), "%d/%m"))),
      div(class = "daily-plan-action", z[[action_col]][[1]]),
      div(class = "daily-plan-amount", amount),
      div(class = "daily-plan-note", z[[note_col]][[1]])
    )
  })
  div(class = "daily-plan-grid", cards)
}
