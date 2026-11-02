options(shiny.maxRequestSize = 30 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(leaflet.extras)
  library(DT)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(digest)
  library(zip)
  library(htmltools)
})

source(file.path("R", "utils.R"), local = TRUE)
source(file.path("R", "app_i18n.R"), local = TRUE)
source(file.path("R", "profile_library.R"), local = TRUE)
source(file.path("R", "fao56_daily_balance.R"), local = TRUE)
source(file.path("R", "layered_soil_balance.R"), local = TRUE)
source(file.path("R", "irrigation_scheduler.R"), local = TRUE)
source(file.path("R", "layered_irrigation_scheduler.R"), local = TRUE)
source(file.path("R", "evidence_gate.R"), local = TRUE)
source(file.path("R", "flow_measurement_assistant.R"), local = TRUE)
source(file.path("R", "forecast_archive.R"), local = TRUE)
source(file.path("R", "soilgrids_gateway.R"), local = TRUE)
source(file.path("R", "soil_profile_integration.R"), local = TRUE)
source(file.path("R", "app_data.R"), local = TRUE)
source(file.path("R", "app_weather.R"), local = TRUE)
source(file.path("R", "app_soil.R"), local = TRUE)
source(file.path("R", "app_engine.R"), local = TRUE)
source(file.path("R", "app_reporting.R"), local = TRUE)
source(file.path("R", "app_ui_helpers.R"), local = TRUE)

app_config <- load_app_config()
registry <- load_system_registry(app_config)
registry_mode <- attr(registry, "registry_mode")
crop_profiles <- load_crop_profiles_daily()
soil_references <- load_soil_profiles()
evidence_levels <- load_evidence_levels()
provenance_classes <- load_provenance_classes()
initial_state_presets <- load_initial_state_presets()
sdg_layer <- load_sdg_layer()

configured_demo_system_id <- trimws(as.character(app_config$scientific_demo_system_id))
default_system_id <- if (configured_demo_system_id %in% registry$public_id) configured_demo_system_id else registry$public_id[[1]]
default_system <- selected_registry_row(registry, default_system_id)

theme <- bslib::bs_theme(
  version = 5,
  bg = "#f4f7f9",
  fg = "#1f2937",
  primary = "#1f6e78",
  secondary = "#17365d",
  success = "#2f7d49",
  warning = "#b7791f",
  danger = "#b42318",
  base_font = "Arial"
)

ui <- fluidPage(
  theme = theme,
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body { background:#f4f7f9; }
      .container-fluid { max-width:1500px; padding-left:22px; padding-right:22px; }
      .hero { background:linear-gradient(135deg,#17365d 0%,#1f6e78 100%); color:white;
              padding:20px 24px; border-radius:16px; margin:14px 0 10px 0;
              box-shadow:0 8px 22px rgba(23,54,93,.18); }
      .hero h1 { font-size:1.8rem; margin:0; font-weight:750; }
      .hero p { margin:5px 0 0 0; opacity:.94; max-width:980px; }
      .alpha-badge { display:inline-block; background:rgba(255,255,255,.14); border:1px solid rgba(255,255,255,.30);
                     border-radius:999px; padding:5px 10px; margin-top:10px; font-size:.84rem; }
      .scope-banner { background:#fff7d6; border-left:6px solid #d4a017; border-radius:8px;
                      padding:10px 14px; margin-bottom:12px; }
      .step-nav { background:white; border-radius:12px; padding:10px 14px; margin-bottom:14px;
                  box-shadow:0 2px 10px rgba(15,23,42,.07); }
      .step-nav .shiny-options-group { display:flex; flex-wrap:wrap; gap:8px; }
      .step-nav .radio-inline { margin:0; padding:8px 13px; border:1px solid #cbd5e1;
                                border-radius:999px; background:#fff; }
      .panel-card { background:white; border-radius:14px; padding:18px; margin-bottom:14px;
                    box-shadow:0 2px 12px rgba(15,23,42,.08); }
      .panel-card h3 { color:#17365d; margin-top:0; font-size:1.2rem; }
      .subtle { color:#64748b; font-size:.89rem; }
      .notice-box { border-left:5px solid; padding:10px 12px; border-radius:7px; margin:8px 0; }
      .notice-info { background:#eaf3fb; border-color:#3b82b6; }
      .notice-warning { background:#fff7d6; border-color:#d4a017; }
      .notice-success { background:#eaf6ea; border-color:#2f7d49; }
      .notice-danger { background:#fdecec; border-color:#b42318; }
      .metric-grid { display:grid; grid-template-columns:repeat(4,minmax(180px,1fr)); gap:12px; margin:12px 0; }
      .metric-card { background:white; border-radius:12px; padding:14px; border:1px solid #e2e8f0;
                     box-shadow:0 2px 8px rgba(15,23,42,.05); }
      .metric-label { color:#64748b; font-size:.82rem; text-transform:uppercase; letter-spacing:.04em; }
      .metric-value { color:#17365d; font-size:1.55rem; font-weight:760; margin-top:2px; }
      .metric-note { color:#64748b; font-size:.82rem; margin-top:3px; }
      .value-tag { display:inline-block; border-radius:999px; padding:4px 9px; font-size:.82rem; font-weight:650; }
      .tag-verified { background:#dcfce7; color:#166534; }
      .tag-modelled { background:#dbeafe; color:#1e40af; }
      .tag-reference { background:#ede9fe; color:#5b21b6; }
      .tag-institutional { background:#e2e8f0; color:#334155; }
      .tag-declared { background:#fef3c7; color:#92400e; }
      .tag-test { background:#fce7f3; color:#9d174d; }
      .tag-missing { background:#fee2e2; color:#991b1b; }
      .tag-neutral { background:#f1f5f9; color:#475569; }
      .leaflet { border-radius:12px; border:1px solid #d7dee7; }
      .system-box { background:#f8fafc; border:1px solid #e2e8f0; padding:12px; border-radius:10px; }
      .action-row { display:flex; flex-wrap:wrap; gap:8px; margin-top:8px; }
      .warning-list li { margin-bottom:6px; }
      .release-box { background:#f8fafc; border-left:5px solid #64748b; padding:12px; border-radius:8px; }
      .download-row { display:flex; flex-wrap:wrap; gap:10px; margin-top:12px; }
      .download-row .btn { margin:0; }
      .shiny-input-container { width:100%; }
      .instruction-card { background:#eef7f8; border-left:6px solid #1f6e78; border-radius:10px;
                          padding:12px 16px; margin:-2px 0 14px 0; box-shadow:0 2px 8px rgba(15,23,42,.05); }
      .instruction-card strong { display:block; color:#17365d; margin-bottom:3px; }
      .search-help { color:#475569; font-size:.9rem; margin-top:-6px; margin-bottom:10px; }
      .search-action-row { margin-top:-4px; margin-bottom:8px; justify-content:flex-end; }
      .search-status { border-radius:8px; padding:8px 10px; margin:6px 0 10px 0; font-size:.9rem; }
      .search-status-info { background:#eef7f8; color:#17365d; }
      .search-status-warning { background:#fff7d6; color:#7c5200; }
      .point-picker-active { background:#eaf3fb; border:1px dashed #3b82b6; color:#17365d;
                             padding:9px 11px; border-radius:8px; margin-top:9px; font-weight:700; }
      .map-legend { background:rgba(255,255,255,.96); padding:10px 12px; border-radius:9px;
                    box-shadow:0 1px 7px rgba(15,23,42,.22); font-size:12px; line-height:1.45; color:#1f2937; }
      .map-legend-title { font-weight:800; color:#17365d; margin-bottom:5px; }
      .legend-symbol { display:inline-block; width:12px; height:12px; margin-right:7px; vertical-align:-1px; }
      .legend-other { border-radius:50%; background:#94a3b8; border:1px solid #475569; }
      .legend-intake { border-radius:50%; background:#2563eb; border:2px solid #17365d; }
      .legend-productive { border-radius:2px; background:#16a34a; border:2px solid #166534; transform:rotate(45deg); }
      .legend-polygon { background:rgba(249,115,22,.22); border:2px solid #ea580c; }
      .evidence-hero { background:#eef7f8; border-left:7px solid #1f6e78; border-radius:12px;
                       padding:14px 16px; margin-bottom:12px; }
      .evidence-hero h2 { color:#17365d; font-size:1.35rem; margin:0 0 5px 0; }
      .evidence-code { color:#64748b; font-size:.82rem; margin-top:6px; }
      .source-summary-grid { display:grid; grid-template-columns:1fr; gap:8px; margin-top:10px; }
      .source-summary-row { display:flex; justify-content:space-between; gap:12px; align-items:flex-start;
                            border:1px solid #e2e8f0; border-radius:9px; padding:9px 10px; background:#f8fafc; }
      .source-summary-label { color:#17365d; font-weight:700; }
      .source-summary-value { color:#475569; text-align:right; }
      .technical-downloads { border-top:1px solid #d7dee7; margin-top:14px; padding-top:12px; }
      .filter-details { margin:10px 0 12px 0; border:1px solid #d7dee7; border-radius:10px; padding:8px 12px; background:#f8fafc; }
      .filter-details summary { cursor:pointer; font-weight:700; color:#17365d; padding:4px 0; }
      .filter-details[open] summary { margin-bottom:10px; }
      .wizard-footer { position:sticky; bottom:0; z-index:900; display:flex; justify-content:space-between;
                       align-items:center; gap:14px; background:rgba(255,255,255,.96); border-top:1px solid #d7dee7;
                       padding:12px 18px; margin:18px -22px 0 -22px; box-shadow:0 -5px 18px rgba(15,23,42,.10); }
      .wizard-footer .btn { min-width:135px; font-weight:700; }
      .wizard-progress { color:#64748b; font-size:.9rem; text-align:center; }
      .action-summary { background:#eaf6ea; border-left:7px solid #2f7d49; border-radius:12px; padding:16px 18px;
                        margin:8px 0 12px 0; box-shadow:0 2px 8px rgba(15,23,42,.05); }
      .action-summary.warning { background:#fff7d6; border-left-color:#d4a017; }
      .action-summary h2 { margin:0 0 5px 0; color:#17365d; font-size:1.3rem; }
      .action-summary .primary-action { font-size:1.15rem; font-weight:760; color:#17365d; }
      .daily-plan-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:10px; }
      .daily-plan-card { border:1px solid #d7dee7; border-radius:12px; padding:12px; background:white; min-height:165px; }
      .daily-action-irrigate { border-top:6px solid #2f7d49; background:#f3fbf5; }
      .daily-action-warning { border-top:6px solid #d4a017; background:#fffaf0; }
      .daily-action-neutral { border-top:6px solid #94a3b8; background:#f8fafc; }
      .daily-plan-date { color:#475569; font-size:.84rem; text-transform:capitalize; }
      .daily-plan-action { color:#17365d; font-size:1.02rem; font-weight:760; margin:5px 0; }
      .daily-plan-amount { font-weight:700; margin-bottom:5px; }
      .daily-plan-note { color:#475569; font-size:.84rem; line-height:1.35; }
      .simple-warning-list li { margin-bottom:7px; }
      .details-box { border:1px solid #d7dee7; border-radius:10px; padding:8px 12px; background:#f8fafc; margin-top:10px; }
      .details-box summary { cursor:pointer; color:#17365d; font-weight:700; }
      .area-source-box { border:1px solid #d7dee7; border-radius:10px; padding:10px 12px; margin-top:10px; background:#f8fafc; }
      .area-source-box .shiny-options-group { margin-bottom:0; }
      .parcel-scale-banner { background:#eef7f8; border-left:6px solid #1f6e78; border-radius:10px; padding:10px 14px; margin:0 0 12px 0; font-weight:700; color:#17365d; }
      @media(max-width:1050px){ .metric-grid{grid-template-columns:repeat(2,minmax(160px,1fr));} }
      @media(max-width:650px){ .metric-grid{grid-template-columns:1fr;} .container-fluid{padding-left:10px;padding-right:10px;} .wizard-footer{margin-left:-10px;margin-right:-10px;} .wizard-footer .btn{min-width:105px;} }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('activatePolygonDraw', function(message) {
        var attempts = 24;
        var activate = function() {
          var button = document.querySelector('.leaflet-draw-draw-polygon');
          if (button) {
            button.click();
            return;
          }
          attempts -= 1;
          if (attempts > 0) { window.setTimeout(activate, 150); }
        };
        window.setTimeout(activate, 100);
      });
    "))
  ),
  div(
    class = "hero",
    fluidRow(
      column(8, uiOutput("hero_text")),
      column(
        4,
        div(
          style = "display:flex;justify-content:flex-end;gap:12px;align-items:flex-start;flex-wrap:wrap;",
          selectInput("language", label = "Idioma", choices = c("Español" = "es", "English" = "en"), selected = "es", width = "140px"),
          checkboxInput("technical_view", "Vista técnica", value = FALSE)
        )
      )
    )
  ),
  div(class = "scope-banner", textOutput("scope_notice")),
  div(
    class = "step-nav",
    radioButtons(
      "step", label = NULL,
      choices = c(
        "1. Seleccione su sistema" = "territory",
        "2. Ingrese sus datos" = "scenario",
        "3. Revise la información" = "evidence",
        "4. Consulte su plan" = "results",
        "5. Guarde el resultado" = "traceability"
      ),
      selected = "territory", inline = TRUE
    )
  ),
  uiOutput("step_instruction_ui"),

  conditionalPanel(
    condition = "input.step == 'territory'",
    fluidRow(
      column(
        4,
        div(
          class = "panel-card",
          h3(textOutput("territory_heading", inline = TRUE)),
          uiOutput("registry_status_ui"),
          textInput(
            "system_search_query", "Escriba el nombre o ID del sistema",
            value = "", placeholder = "Ejemplo: nombre del sistema o ID"
          ),
          div(
            class = "action-row search-action-row",
            actionButton("clear_system_search", "Limpiar búsqueda", class = "btn-outline-secondary btn-sm")
          ),
          p(class = "search-help", textOutput("system_search_help")),
          uiOutput("system_search_status_ui"),
          selectInput(
            "system_public_id", "Coincidencias encontradas",
            choices = registry_choice_vector(registry), selected = default_system_id
          ),
          tags$details(
            class = "filter-details",
            tags$summary(textOutput("alternative_search_label", inline = TRUE)),
            selectInput("canton_filter", "Cantón", choices = c("Todos los cantones" = "__ALL__"), selected = "__ALL__"),
            selectInput("parish_filter", "Parroquia", choices = c("Todas las parroquias" = "__ALL__"), selected = "__ALL__"),
            p(class = "subtle", textOutput("parish_filter_note")),
            uiOutput("registry_filter_count_ui"),
            actionButton("clear_admin_filters", "Mostrar todos los sistemas", class = "btn-outline-secondary btn-sm")
          ),
          conditionalPanel("input.technical_view == true", uiOutput("administrative_labels_note_ui")),
          uiOutput("selected_system_ui"),
          hr(),
          h4(textOutput("productive_heading", inline = TRUE)),
          uiOutput("productive_status_ui"),
          div(
            class = "action-row",
            actionButton("activate_point_picker", "Marcar mi parcela en el mapa", class = "btn-primary"),
            actionButton("use_intake", "Usar captación como aproximación (solo demostración)", class = "btn-outline-warning"),
            actionButton("clear_productive", "Borrar ubicación productiva", class = "btn-outline-secondary")
          ),
          uiOutput("point_picker_status_ui"),
          actionButton("activate_polygon_draw", "Dibujar el terreno sembrado", class = "btn-outline-primary"),
          uiOutput("polygon_crop_context_ui"),
          uiOutput("polygon_status_ui"),
          uiOutput("polygon_area_choice_ui"),
          conditionalPanel(
            "input.technical_view == true",
            hr(),
            numericInput("manual_latitude", "Latitud", value = default_system$latitude[[1]], step = 0.000001),
            numericInput("manual_longitude", "Longitud", value = default_system$longitude[[1]], step = 0.000001),
            numericInput("productive_elevation_m", "Elevación productiva o de referencia (m)", value = default_system$altitude_m[[1]], step = 1),
            actionButton("apply_manual_point", "Aplicar coordenadas manuales", class = "btn-outline-primary")
          ),
          p(class = "subtle", textOutput("map_instruction"))
        )
      ),
      column(
        8,
        div(class = "panel-card", leafletOutput("territory_map", height = "650px"))
      )
    )
  ),

  conditionalPanel(
    condition = "input.step == 'scenario'",
    fluidRow(
      column(
        6,
        div(
          class = "panel-card",
          h3(textOutput("crop_heading", inline = TRUE)),
          selectInput("crop_profile_id", "Cultivo", choices = character(0), selected = "papa_superchola"),
          radioButtons(
            "crop_age_mode", "Cómo definir la etapa",
            choices = c("Edad del cultivo" = "crop_age", "Fecha de siembra/trasplante" = "planting_date"),
            selected = "crop_age", inline = TRUE
          ),
          conditionalPanel("input.crop_age_mode == 'crop_age'", numericInput("crop_age_days", "Edad del cultivo (días)", value = 60, min = 1, step = 1)),
          conditionalPanel("input.crop_age_mode == 'planting_date'", dateInput("planting_date", "Fecha de siembra/trasplante", value = Sys.Date() - 59)),
          fluidRow(
            column(6, numericInput("area_ha", "Área sembrada con el cultivo seleccionado (ha)", value = 0.25, min = 0.0001, step = 0.01)),
            column(6, numericInput("delivery_efficiency", "Eficiencia de entrega/aplicación", value = 0.75, min = 0.01, max = 1, step = 0.01))
          ),
          uiOutput("area_explanation_ui"),
          hr(),
          h3(textOutput("turn_heading", inline = TRUE)),
          fluidRow(
            column(6, dateInput("current_turn_date", "Fecha del turno actual", value = Sys.Date() + 1)),
            column(6, dateInput("next_turn_date", "Fecha del siguiente turno", value = Sys.Date() + 8))
          ),
          uiOutput("turn_horizon_note_ui"),
          fluidRow(
            column(6, numericInput("turn_hours", "Horas disponibles en el turno", value = 6, min = 0, step = 0.25)),
            column(6, numericInput("flow_lps", "Caudal activo (L/s)", value = 2, min = 0.001, step = 0.1))
          ),
          selectInput(
            "flow_source", "Fuente del caudal",
            choices = c(
              "Declarado por el usuario" = "declared",
              "Referencia institucional/autorizada" = "institutional",
              "Medidor instalado validado" = "meter",
              "Aforo volumétrico documentado" = "volumetric",
              "Estructura hidráulica calibrada" = "calibrated_structure",
              "Otro método documentado" = "other_documented"
            ),
            selected = "declared"
          ),
          uiOutput("flow_context_ui"),
          checkboxInput("turn_schedule_verified", "Calendario de turnos verificado con la organización", value = FALSE),
          selectInput(
            "recent_irrigation_history_status", "Historial reciente de riego",
            choices = c(
              "No disponible" = "unavailable",
              "Declarado por el usuario" = "declared",
              "Registro local documentado" = "documented"
            ),
            selected = "unavailable"
          )
        )
      ),
      column(
        6,
        div(
          class = "panel-card",
          h3(textOutput("evidence_inputs_heading", inline = TRUE)),
          uiOutput("support_data_help_ui"),
          selectInput(
            "weather_mode", "Fuente meteorológica",
            choices = c(
              "Datos meteorológicos de demostración (sin internet)" = "offline_fixture",
              "Pronóstico guardado para demostración (no es actual)" = "archived_fixture",
              "Archivo meteorológico propio o de estación local" = "manual_csv",
              "Pronóstico actual en línea" = "live_openmeteo"
            ),
            selected = "offline_fixture"
          ),
          conditionalPanel(
            "input.weather_mode == 'manual_csv'",
            uiOutput("weather_file_ui")
          ),
          uiOutput("weather_mode_note_ui"),
          selectInput(
            "soil_mode", "Fuente del suelo",
            choices = c(
              "Referencia pública de suelo guardada para Santul" = "archived_santul",
              "Suelo general de referencia" = "fao_reference",
              "Información local del suelo" = "local_uniform",
              "Consultar referencia pública de suelo para esta ubicación" = "live_soilgrids"
            ),
            selected = if (configured_demo_system_id %in% registry$public_id) "archived_santul" else "fao_reference"
          ),
          conditionalPanel(
            "input.soil_mode == 'fao_reference'",
            selectInput("reference_soil_id", "Clase de suelo de referencia", choices = character(0), selected = "medium_silt")
          ),
          conditionalPanel(
            "input.soil_mode == 'local_uniform'",
            fluidRow(
              column(6, numericInput("local_theta_fc", "θFC", value = 0.32, min = 0.01, max = 0.8, step = 0.001)),
              column(6, numericInput("local_theta_wp", "θWP", value = 0.15, min = 0, max = 0.7, step = 0.001))
            ),
            checkboxInput("local_soil_verified", "Perfil local verificado/documentado", value = FALSE)
          ),
          hr(),
          h4(textOutput("initial_heading", inline = TRUE)),
          selectInput(
            "initial_condition_simple", "¿Cómo está la humedad del suelo hoy?",
            choices = c(
              "Húmedo o recién regado" = "wet",
              "Humedad intermedia" = "medium",
              "Seco, cerca de necesitar riego" = "dry",
              "Muy seco; posible estrés" = "very_dry",
              "Ingresar un valor técnico" = "technical"
            ), selected = "dry"
          ),
          p(class = "subtle", textOutput("initial_simple_note")),
          conditionalPanel(
            "input.initial_condition_simple == 'technical'",
            selectInput(
              "initial_mode", "Forma de ingreso",
              choices = c(
                "Fracción de RAW agotada" = "fraction_raw",
                "Fracción de TAW agotada" = "fraction_taw",
                "Agotamiento en mm" = "mm",
                "Capacidad de campo asumida" = "field_capacity",
                "Contenido volumétrico medido (θ)" = "measured_theta"
              ), selected = "fraction_raw"
            ),
            numericInput("initial_value", "Valor", value = 0.90, min = 0, step = 0.05),
            checkboxInput("initial_water_verified", "Estado hídrico medido o verificado", value = FALSE)
          ),
          conditionalPanel(
            "input.technical_view == true",
            tags$details(
              class = "details-box",
              tags$summary(textOutput("technical_balance_label", inline = TRUE)),
              selectInput(
                "subsoil_mode", "Agua en suelo incorporado por crecimiento radicular",
                choices = c(
                  "Asumir capacidad de campo" = "field_capacity",
                  "Misma fracción relativa de agotamiento" = "same_relative_depletion",
                  "Contenido volumétrico medido" = "measured_theta"
                ), selected = "field_capacity"
              ),
              conditionalPanel(
                "input.subsoil_mode == 'measured_theta'",
                numericInput("subsoil_theta_value", "θ del subsuelo", value = 0.25, min = 0, max = 0.8, step = 0.001),
                checkboxInput("subsoil_verified", "Medición de subsuelo verificada", value = FALSE)
              ),
              numericInput("runoff_fraction", "Fracción de escorrentía", value = 0, min = 0, max = 0.99, step = 0.01)
            )
          ),
          conditionalPanel(
            "input.technical_view == true",
            div(
              class = "action-row",
              actionButton("calculate_plan", "Calcular mi plan", class = "btn-primary btn-lg"),
              actionButton("reset_scenario", "Restablecer escenario", class = "btn-outline-secondary")
            )
          )
        )
      )
    )
  ),

  conditionalPanel(
    condition = "input.step == 'evidence'",
    fluidRow(
      column(
        5,
        div(
          class = "panel-card",
          h3(textOutput("evidence_heading", inline = TRUE)),
          uiOutput("parcel_scale_ui"),
          uiOutput("readiness_ui"),
          uiOutput("simple_evidence_summary_ui"),
          tags$details(
            class = "details-box",
            tags$summary(textOutput("readiness_details_label", inline = TRUE)),
            uiOutput("readiness_boundary_ui")
          ),
          conditionalPanel("input.technical_view == true", DTOutput("evidence_framework_table"))
        )
      ),
      column(
        7,
        div(
          class = "panel-card",
          h3(textOutput("provenance_heading", inline = TRUE)),
          uiOutput("provenance_empty_ui"),
          uiOutput("simple_provenance_ui"),
          conditionalPanel("input.technical_view == true", DTOutput("provenance_table"))
        )
      )
    )
  ),

  conditionalPanel(
    condition = "input.step == 'results'",
    uiOutput("no_result_ui"),
    uiOutput("parcel_scale_results_ui"),
    uiOutput("action_summary_ui"),
    uiOutput("result_metrics_ui"),
    div(
      class = "panel-card",
      h3(textOutput("daily_plan_heading", inline = TRUE)),
      uiOutput("daily_plan_note_ui"),
      uiOutput("daily_plan_ui")
    ),
    div(
      class = "panel-card",
      h3(textOutput("priority_warnings_heading", inline = TRUE)),
      uiOutput("priority_warnings_ui")
    ),
    conditionalPanel(
      "input.technical_view == true",
      fluidRow(
        column(7, div(class = "panel-card", h3(textOutput("range_heading", inline = TRUE)), plotOutput("scenario_range_plot", height = "380px"))),
        column(5, div(class = "panel-card", h3(textOutput("warnings_heading", inline = TRUE)), uiOutput("warnings_ui")))
      ),
      div(
        class = "panel-card",
        h3(textOutput("technical_heading", inline = TRUE)),
        tabsetPanel(
          tabPanel("Escenarios / Scenarios", DTOutput("scenario_table")),
          tabPanel("Balance diario / Daily balance", plotOutput("daily_balance_plot", height = "430px"), DTOutput("daily_balance_table")),
          tabPanel("Turnos / Turns", DTOutput("event_table")),
          tabPanel("Suelo / Soil", DTOutput("soil_table")),
          tabPanel("Diagnósticos / Diagnostics", DTOutput("diagnostics_table"))
        )
      )
    )
  ),

  conditionalPanel(
    condition = "input.step == 'traceability'",
    fluidRow(
      column(
        5,
        div(
          class = "panel-card",
          h3(textOutput("release_heading", inline = TRUE)),
          div(class = "release-box", textOutput("release_text")),
          uiOutput("traceability_note_ui"),
          uiOutput("download_buttons_ui")
        )
      ),
      column(
        7,
        div(
          class = "panel-card",
          h3(textOutput("sdg_heading", inline = TRUE)),
          uiOutput("sdg_boundary_ui"),
          DTOutput("sdg_table")
        )
      )
    )
  ),
  div(
    class = "wizard-footer",
    actionButton("nav_back", "Atrás", class = "btn-outline-secondary"),
    div(class = "wizard-progress", textOutput("wizard_progress")),
    actionButton("nav_next", "Siguiente", class = "btn-primary")
  )
)

server <- function(input, output, session) {
  state <- reactiveValues(
    selected_system_id = default_system_id,
    pending_canton = NULL,
    pending_parish = NULL,
    productive_latitude = NULL,
    productive_longitude = NULL,
    productive_location_source = NULL,
    productive_polygon = NULL,
    polygon_metrics = NULL,
    polygon_crop_profile_id = NULL,
    polygon_draw_active = FALSE,
    area_source_choice = "declared",
    point_pick_active = FALSE,
    result = NULL,
    error = NULL
  )

  lang <- reactive(if (identical(input$language, "en")) "en" else "es")
  tx <- function(key, ...) tr_app(key, lang(), ...)
  productive_source_label <- function(code) {
    switch(
      as.character(code %||% ""),
      intake_approximation = tx("productive_source_intake"),
      map_click = tx("productive_source_map"),
      polygon_centroid = if (lang() == "en") "polygon-derived representative location" else "ubicación representativa calculada del polígono",
      manual = tx("productive_source_manual"),
      as.character(code %||% "")
    )
  }

  selected_system <- reactive({
    sid <- resolve_registry_selection(state$selected_system_id, registry, default_system_id)
    selected_registry_row(registry, sid)
  })

  active_crop_name <- reactive({
    crop_id <- as.character(input$crop_profile_id %||% "papa_superchola")
    row <- crop_profiles[crop_profiles$crop_profile_id == crop_id, , drop = FALSE]
    if (nrow(row) != 1L) row <- crop_profiles[crop_profiles$crop_profile_id == "papa_superchola", , drop = FALSE]
    if (!nrow(row)) return(crop_id)
    if (identical(lang(), "en")) row$crop_name_en[[1]] else row$crop_name_es[[1]]
  })

  filtered_registry <- reactive({
    filter_system_registry(
      registry,
      canton = input$canton_filter,
      parish = input$parish_filter
    )
  })

  system_search_matches <- reactive({
    query <- trimws(as.character(input$system_search_query %||% ""))
    if (nzchar(query)) {
      search_system_registry(registry, query, max_results = 25L)
    } else {
      filtered_registry()
    }
  })

  output$hero_text <- renderUI({
    tagList(
      h1(tx("app_title")),
      p(tx("app_subtitle")),
      div(class = "alpha-badge", tx("alpha_badge"))
    )
  })
  output$scope_notice <- renderText(tx("scope_notice"))
  output$step_instruction_ui <- renderUI({
    step <- input$step %||% "territory"
    keys <- switch(
      step,
      territory = c("territory_instruction_title", "territory_instruction"),
      scenario = c("scenario_instruction_title", "scenario_instruction"),
      evidence = c("evidence_instruction_title", "evidence_instruction"),
      results = c("results_instruction_title", "results_instruction"),
      traceability = c("traceability_instruction_title", "traceability_instruction"),
      c("territory_instruction_title", "territory_instruction")
    )
    div(class = "instruction-card", tags$strong(tx(keys[[1]])), tx(keys[[2]]))
  })
  output$system_search_help <- renderText(tx("system_search_help"))
  output$alternative_search_label <- renderText(tx("alternative_search"))
  output$parish_filter_note <- renderText(tx("parish_filter_note"))
  output$readiness_details_label <- renderText(if (lang() == "en") "What does this evidence code mean?" else "¿Qué significa este código de evidencia?")
  output$initial_simple_note <- renderText(tx("initial_simple_note"))
  output$technical_balance_label <- renderText(tx("technical_balance_options"))
  output$daily_plan_heading <- renderText(tx("seven_day_plan"))
  output$priority_warnings_heading <- renderText(tx("priority_warnings"))
  output$support_data_help_ui <- renderUI(notice_box_ui(tx("support_data_help"), "info"))
  output$turn_horizon_note_ui <- renderUI(notice_box_ui(tx("turn_horizon_note"), "info"))
  output$wizard_progress <- renderText({
    order <- c("territory", "scenario", "evidence", "results", "traceability")
    idx <- match(input$step %||% "territory", order)
    if (lang() == "en") paste0("Step ", idx, " of ", length(order)) else paste0("Paso ", idx, " de ", length(order))
  })
  output$territory_heading <- renderText(tx("step_territory"))
  output$productive_heading <- renderText(tx("productive_location"))
  output$crop_heading <- renderText(tx("crop"))
  output$turn_heading <- renderText(if (lang() == "en") "Flow and turn schedule" else "Caudal y calendario de turnos")
  output$evidence_inputs_heading <- renderText(tx("support_data_heading"))
  output$initial_heading <- renderText(tx("initial_water"))
  output$evidence_heading <- renderText(tx("evidence_title"))
  output$provenance_heading <- renderText(tx("provenance_title"))
  output$range_heading <- renderText(tx("weather_range"))
  output$warnings_heading <- renderText(tx("warnings"))
  output$technical_heading <- renderText(tx("technical_diagnostics"))
  output$release_heading <- renderText(tx("release_status"))
  output$sdg_heading <- renderText(tx("sdg_layer"))
  output$map_instruction <- renderText(tx("click_map_note"))
  output$area_explanation_ui <- renderUI({
    declared <- suppressWarnings(as.numeric(input$area_ha %||% NA_real_))
    active <- if (!is.null(state$polygon_metrics) && identical(state$area_source_choice, "polygon")) {
      as.numeric(state$polygon_metrics$area_ha)
    } else declared
    source_label <- if (!is.null(state$polygon_metrics) && identical(state$area_source_choice, "polygon")) {
      tx("area_source_polygon_short")
    } else {
      tx("area_source_declared_short")
    }
    tagList(
      notice_box_ui(tx("area_crop_note"), "info"),
      if (is.null(state$polygon_metrics)) notice_box_ui(tx("point_area_note"), "warning") else notice_box_ui(tx("polygon_calculation_note"), "warning"),
      if (is.finite(active)) div(class = "system-box", tags$strong(tx("active_area_status", active, source_label))) else NULL,
      p(class = "subtle", tx("mixed_crop_boundary"))
    )
  })
  output$polygon_crop_context_ui <- renderUI({
    tagList(
      p(class = "subtle", tags$strong(tx("polygon_selected_crop", active_crop_name()))),
      p(class = "subtle", tx("polygon_crop_boundary"))
    )
  })
  output$parcel_scale_ui <- renderUI({
    if (is.null(state$result)) return(NULL)
    div(class = "parcel-scale-banner", tx("parcel_scale_statement"))
  })
  output$parcel_scale_results_ui <- renderUI({
    if (is.null(state$result)) return(NULL)
    div(class = "parcel-scale-banner", tx("parcel_scale_statement"))
  })
  output$weather_file_ui <- renderUI({
    tagList(
      fileInput("weather_file", tx("weather_file"), accept = c(".csv", "text/csv")),
      downloadButton("download_weather_template", tx("weather_template"), class = "btn-outline-secondary")
    )
  })
  output$download_buttons_ui <- renderUI({
    tagList(
      div(
        class = "download-row",
        downloadButton("download_user_excel", tx("export_excel"), class = "btn-primary"),
        downloadButton("download_report", tx("export_report"), class = "btn-outline-primary")
      ),
      if (isTRUE(input$technical_view)) {
        div(
          class = "technical-downloads",
          tags$strong(tx("technical_downloads")),
          div(
            class = "download-row",
            downloadButton("download_daily_plan", tx("export_daily_plan"), class = "btn-outline-secondary"),
            downloadButton("download_results_csv", tx("export_results"), class = "btn-outline-secondary"),
            downloadButton("download_bundle", tx("export_bundle"), class = "btn-success"),
            downloadButton("download_rds", tx("save_session"), class = "btn-outline-secondary")
          )
        )
      } else NULL
    )
  })
  output$flow_context_ui <- renderUI({
    measured_sources <- c("meter", "volumetric", "calibrated_structure", "other_documented")
    flow_source <- input$flow_source %||% "declared"
    if (!flow_source %in% measured_sources) {
      return(notice_box_ui(tx("flow_context_not_measurement"), "info"))
    }
    tagList(
      hr(),
      h4(tx("flow_context_heading")),
      fluidRow(
        column(6, dateInput("flow_measurement_date", tx("flow_measurement_date"), value = isolate(input$flow_measurement_date %||% Sys.Date()))),
        column(6, numericInput("flow_repetition_count", tx("flow_repetition_count"), value = isolate(input$flow_repetition_count %||% if (identical(flow_source, "volumetric")) 2 else 1), min = 1, step = 1))
      ),
      textInput("flow_measurement_location", tx("flow_measurement_location"), value = isolate(input$flow_measurement_location %||% "")),
      textInput("flow_measurement_method_detail", tx("flow_measurement_method_detail"), value = isolate(input$flow_measurement_method_detail %||% "")),
      checkboxInput("flow_context_documented", tx("flow_context_documented"), value = isolate(isTRUE(input$flow_context_documented))),
      checkboxInput("flow_verified", tx("flow_verified"), value = isolate(isTRUE(input$flow_verified))),
      p(class = "subtle", tx("flow_context_help"))
    )
  })

  observe({
    l <- lang()
    current_step <- isolate(input$step %||% "territory")
    updateSelectInput(session, "language", label = tr_app("language", l))
    updateCheckboxInput(session, "technical_view", label = tr_app("technical_view", l), value = isolate(input$technical_view %||% FALSE))
    updateRadioButtons(
      session, "step", label = NULL,
      choices = stats::setNames(
        c("territory", "scenario", "evidence", "results", "traceability"),
        vapply(c("step_territory", "step_scenario", "step_evidence", "step_results", "step_traceability"), tr_app, character(1), language = l)
      ),
      selected = current_step, inline = TRUE
    )
    updateSelectInput(session, "canton_filter", label = tr_app("canton", l))
    updateSelectInput(session, "parish_filter", label = tr_app("parish", l))
    updateTextInput(
      session, "system_search_query",
      label = tr_app("system_search_query", l),
      value = isolate(input$system_search_query %||% ""),
      placeholder = tr_app("system_search_placeholder", l)
    )
    updateSelectInput(session, "system_public_id", label = tr_app("system_search_results", l))
    updateActionButton(session, "clear_system_search", label = tr_app("clear_system_search", l))
    updateActionButton(session, "clear_admin_filters", label = tr_app("clear_filters", l))
    updateNumericInput(session, "manual_latitude", label = tr_app("latitude", l))
    updateNumericInput(session, "manual_longitude", label = tr_app("longitude", l))
    updateNumericInput(session, "productive_elevation_m", label = if (l == "en") "Productive or reference elevation (m)" else "Elevación productiva o de referencia (m)")
    updateActionButton(session, "activate_point_picker", label = tr_app("mark_productive_point", l))
    updateActionButton(session, "use_intake", label = tr_app("use_intake_approx", l))
    updateActionButton(session, "clear_productive", label = tr_app("clear_productive", l))
    updateActionButton(session, "activate_polygon_draw", label = if (is.null(state$productive_polygon)) tr_app("polygon_draw", l) else tr_app("polygon_redraw", l))
    updateActionButton(session, "apply_manual_point", label = tr_app("apply_manual_point", l))

    crop_choices <- stats::setNames(
      crop_profiles$crop_profile_id,
      if (l == "en") paste0(crop_profiles$crop_name_en, " — ", crop_profiles$scientific_name) else paste0(crop_profiles$crop_name_es, " — ", crop_profiles$scientific_name)
    )
    updateSelectInput(session, "crop_profile_id", label = tr_app("crop", l), choices = crop_choices, selected = isolate(input$crop_profile_id %||% "papa_superchola"))
    updateRadioButtons(
      session, "crop_age_mode", label = tr_app("crop_age_mode", l),
      choices = stats::setNames(c("crop_age", "planting_date"), c(tr_app("crop_age", l), tr_app("planting_date", l))),
      selected = isolate(input$crop_age_mode %||% "crop_age"), inline = TRUE
    )
    updateNumericInput(session, "crop_age_days", label = tr_app("crop_age", l))
    updateDateInput(session, "planting_date", label = tr_app("planting_date", l))
    updateNumericInput(session, "area_ha", label = tr_app("area", l))
    updateNumericInput(session, "delivery_efficiency", label = tr_app("efficiency", l))
    updateDateInput(session, "current_turn_date", label = tr_app("turn_date", l))
    updateDateInput(session, "next_turn_date", label = tr_app("next_turn_date", l))
    updateNumericInput(session, "turn_hours", label = tr_app("turn_hours", l))
    updateNumericInput(session, "flow_lps", label = tr_app("flow", l))
    updateSelectInput(
      session, "flow_source", label = tr_app("flow_source", l),
      choices = stats::setNames(
        c("declared", "institutional", "meter", "volumetric", "calibrated_structure", "other_documented"),
        vapply(c("flow_declared", "flow_institutional", "flow_meter", "flow_volumetric", "flow_structure", "flow_other"), tr_app, character(1), language = l)
      ), selected = isolate(input$flow_source %||% "declared")
    )
    updateCheckboxInput(session, "turn_schedule_verified", label = tr_app("turn_schedule_verified", l), value = isolate(input$turn_schedule_verified %||% FALSE))
    updateSelectInput(
      session, "recent_irrigation_history_status", label = tr_app("recent_history", l),
      choices = stats::setNames(
        c("unavailable", "declared", "documented"),
        vapply(c("recent_history_unavailable", "recent_history_declared", "recent_history_documented"), tr_app, character(1), language = l)
      ), selected = isolate(input$recent_irrigation_history_status %||% "unavailable")
    )
    updateSelectInput(
      session, "weather_mode", label = tr_app("weather_source", l),
      choices = stats::setNames(
        c("offline_fixture", "archived_fixture", "manual_csv", "live_openmeteo"),
        vapply(c("weather_offline", "weather_archived", "weather_manual", "weather_live"), tr_app, character(1), language = l)
      ), selected = isolate(input$weather_mode %||% "offline_fixture")
    )
    updateSelectInput(
      session, "soil_mode", label = tr_app("soil_source", l),
      choices = stats::setNames(
        c("archived_santul", "fao_reference", "local_uniform", "live_soilgrids"),
        vapply(c("soil_archived_santul", "soil_fao_reference", "soil_local", "soil_live"), tr_app, character(1), language = l)
      ), selected = isolate(input$soil_mode %||% "fao_reference")
    )
    ref_choices <- stats::setNames(
      soil_references$soil_profile_id,
      if (l == "en") soil_references$soil_name_en else soil_references$soil_name_es
    )
    updateSelectInput(session, "reference_soil_id", label = tr_app("soil_reference_profile", l), choices = ref_choices,
                      selected = isolate(input$reference_soil_id %||% "medium_silt"))
    updateCheckboxInput(session, "local_soil_verified", label = tr_app("local_soil_verified", l), value = isolate(input$local_soil_verified %||% FALSE))
    updateSelectInput(
      session, "initial_condition_simple", label = tr_app("initial_simple", l),
      choices = stats::setNames(
        c("wet", "medium", "dry", "very_dry", "technical"),
        vapply(c("initial_simple_wet", "initial_simple_medium", "initial_simple_dry", "initial_simple_very_dry", "initial_simple_technical"), tr_app, character(1), language = l)
      ),
      selected = isolate(input$initial_condition_simple %||% "dry")
    )
    updateSelectInput(
      session, "initial_mode", label = tr_app("initial_mode", l),
      choices = stats::setNames(
        c("fraction_raw", "fraction_taw", "mm", "field_capacity", "measured_theta"),
        vapply(c("initial_fraction_raw", "initial_fraction_taw", "initial_mm", "initial_field_capacity", "initial_measured_theta"), tr_app, character(1), language = l)
      ), selected = isolate(input$initial_mode %||% "fraction_raw")
    )
    updateNumericInput(session, "initial_value", label = tr_app("initial_value", l))
    updateCheckboxInput(session, "initial_water_verified", label = tr_app("initial_verified", l), value = isolate(input$initial_water_verified %||% FALSE))
    updateSelectInput(
      session, "subsoil_mode", label = tr_app("subsoil_mode", l),
      choices = stats::setNames(c("field_capacity", "same_relative_depletion", "measured_theta"),
                                vapply(c("subsoil_fc", "subsoil_same", "subsoil_measured"), tr_app, character(1), language = l)),
      selected = isolate(input$subsoil_mode %||% "field_capacity")
    )
    updateNumericInput(session, "subsoil_theta_value", label = tr_app("subsoil_theta", l))
    updateNumericInput(session, "runoff_fraction", label = tr_app("runoff_fraction", l))
    updateActionButton(session, "calculate_plan", label = tr_app("calculate", l))
    updateActionButton(session, "reset_scenario", label = tr_app("reset", l))
    updateActionButton(session, "nav_back", label = tr_app("back", l))
    next_label <- if (identical(current_step, "scenario")) tr_app("next_calculate", l) else tr_app("next_step", l)
    updateActionButton(session, "nav_next", label = next_label)
  })

  # Administrative synchronisation is staged because Shiny input updates are asynchronous.
  # The canton must be acknowledged by the browser before the parish choices are rebuilt.
  observe({
    canton_values <- registry_filter_choices(registry$canton_display)
    canton_choices <- c("__ALL__", canton_values)
    canton_labels <- c(tx("all_cantons"), canton_values)
    pending <- state$pending_canton
    current <- if (!is.null(pending) && pending %in% canton_choices) {
      pending
    } else {
      normalise_registry_filter(isolate(input$canton_filter))
    }
    if (!current %in% canton_choices) current <- "__ALL__"
    updateSelectInput(session, "canton_filter", choices = stats::setNames(canton_choices, canton_labels), selected = current)
  })

  observe({
    active_canton <- normalise_registry_filter(input$canton_filter)
    pending_canton <- state$pending_canton
    pending_parish <- state$pending_parish

    # Wait for the browser to confirm the selected system's canton. Rebuilding the
    # parish selector against the previous canton was the cause of the alpha10 race.
    if (!is.null(pending_canton) && !identical(active_canton, pending_canton)) return()

    x <- filter_system_registry(registry, canton = active_canton, parish = "__ALL__")
    parish_values <- registry_filter_choices(x$parish_display)
    parish_choices <- c("__ALL__", parish_values)
    parish_labels <- c(tx("all_parishes"), parish_values)
    current <- if (!is.null(pending_parish) && pending_parish %in% parish_choices) {
      pending_parish
    } else {
      normalise_registry_filter(isolate(input$parish_filter))
    }
    if (!current %in% parish_choices) current <- "__ALL__"
    updateSelectInput(session, "parish_filter", choices = stats::setNames(parish_choices, parish_labels), selected = current)
  })

  # Clear the staged target only after both browser inputs report the intended values.
  observe({
    pending_canton <- state$pending_canton
    pending_parish <- state$pending_parish
    if (is.null(pending_canton) || is.null(pending_parish)) return()
    active_canton <- normalise_registry_filter(input$canton_filter)
    active_parish <- normalise_registry_filter(input$parish_filter)
    if (identical(active_canton, pending_canton) && identical(active_parish, pending_parish)) {
      state$pending_canton <- NULL
      state$pending_parish <- NULL
    }
  })

  observe({
    query <- trimws(as.character(input$system_search_query %||% ""))
    matches <- system_search_matches()
    current <- resolve_registry_selection(state$selected_system_id, registry, default_system_id)
    if (nzchar(query)) {
      rows <- matches
      exact <- system_search_exact_match(registry, query)
      selected <- if (length(exact) == 1L) {
        exact[[1]]
      } else if (nrow(matches) == 1L) {
        matches$public_id[[1]]
      } else if (current %in% matches$public_id) {
        current
      } else {
        character(0)
      }
    } else {
      rows <- matches
      if (!nrow(rows)) rows <- selected_registry_row(registry, current)
      selected <- if (current %in% rows$public_id) current else rows$public_id[[1]]
    }
    updateSelectInput(
      session, "system_public_id",
      label = tx("system_search_results"),
      choices = registry_choice_vector(rows),
      selected = selected
    )
  })

  output$system_search_status_ui <- renderUI({
    query <- trimws(as.character(input$system_search_query %||% ""))
    if (!nzchar(query)) return(NULL)
    n <- nrow(system_search_matches())
    if (!n) {
      return(div(class = "search-status search-status-warning", tx("system_search_no_results")))
    }
    text <- if (n == 1L) tx("system_search_one_result") else tx("system_search_many_results", n)
    div(class = "search-status search-status-info", text)
  })

  output$registry_filter_count_ui <- renderUI({
    n <- nrow(filtered_registry())
    p(class = "subtle", tx("filtered_systems_count", n))
  })

  observeEvent(input$clear_system_search, {
    updateTextInput(session, "system_search_query", value = "")
  })

  observeEvent(input$clear_admin_filters, {
    state$pending_canton <- NULL
    state$pending_parish <- NULL
    updateSelectInput(session, "canton_filter", selected = "__ALL__")
    updateSelectInput(session, "parish_filter", selected = "__ALL__")
  })

  observeEvent(input$system_public_id, {
    sid <- as.character(input$system_public_id %||% "")
    if (!nzchar(sid) || !sid %in% registry$public_id) return()
    state$selected_system_id <- sid
    s <- selected_registry_row(registry, sid)
    desired_canton <- if (nzchar(s$canton_display[[1]])) s$canton_display[[1]] else "__ALL__"
    desired_parish <- if (nzchar(s$parish_display[[1]])) s$parish_display[[1]] else "__ALL__"
    parish_rows <- filter_system_registry(registry, canton = desired_canton, parish = "__ALL__")
    parish_values <- registry_filter_choices(parish_rows$parish_display)
    if (!desired_parish %in% c("__ALL__", parish_values)) desired_parish <- "__ALL__"

    # Stage the administrative target. The parish observer waits until the client
    # has acknowledged the canton, then rebuilds and selects the correct parish.
    state$pending_canton <- desired_canton
    state$pending_parish <- desired_parish
    canton_values <- registry_filter_choices(registry$canton_display)
    updateSelectInput(
      session, "canton_filter",
      choices = stats::setNames(c("__ALL__", canton_values), c(tx("all_cantons"), canton_values)),
      selected = desired_canton
    )
    updateNumericInput(session, "manual_latitude", value = s$latitude[[1]])
    updateNumericInput(session, "manual_longitude", value = s$longitude[[1]])
    if (is.finite(s$altitude_m[[1]])) updateNumericInput(session, "productive_elevation_m", value = s$altitude_m[[1]])
    if (!identical(s$public_id[[1]], configured_demo_system_id) && identical(input$soil_mode, "archived_santul")) {
      updateSelectInput(session, "soil_mode", selected = "fao_reference")
      showNotification(
        if (lang() == "en") "The saved Santul soil reference is location-specific; a general reference soil was selected." else "La referencia de suelo guardada para Santul es específica de ese punto; se seleccionó un suelo general de referencia.",
        type = "warning", duration = 7
      )
    }
  }, ignoreInit = FALSE)

  output$registry_status_ui <- renderUI({
    text <- if (identical(registry_mode, "restricted")) tx("registry_mode_restricted") else tx("registry_mode_demo")
    notice_box_ui(text, if (identical(registry_mode, "restricted")) "success" else "warning")
  })

  output$administrative_labels_note_ui <- renderUI({
    notice_box_ui(tx("administrative_labels_note"), "info")
  })

  output$selected_system_ui <- renderUI({
    s <- selected_system()
    parish_source <- registry_text_or_blank(s$parish_source[[1]])
    combined_parish <- grepl("[/;]", parish_source) || grepl("(^|[[:space:]])y([[:space:]]|$)", tolower(parish_source))
    tagList(
      div(
        class = "system-box",
        tags$strong(paste0(tx("public_id"), ": ")), s$public_id[[1]], tags$br(),
        tags$strong(paste0(tx("selected_system"), ": ")), htmlEscape(s$system_name[[1]]), tags$br(),
        {
          location_parts <- c(s$canton_display[[1]], s$parish_display[[1]])
          location_parts <- location_parts[nzchar(location_parts)]
          if (length(location_parts)) tags$span(class = "subtle", htmlEscape(paste(location_parts, collapse = " / "))) else NULL
        }, tags$br(),
        tags$span(class = "subtle", paste0(tx("intake_location"), ": ", s$latitude[[1]], ", ", s$longitude[[1]]))
      ),
      if (combined_parish) notice_box_ui(tx("combined_parish_warning"), "warning") else NULL
    )
  })

  output$productive_status_ui <- renderUI({
    if (is.null(state$productive_latitude) || is.null(state$productive_longitude)) {
      return(notice_box_ui(tx("productive_location_missing"), "warning"))
    }
    s <- selected_system()
    distance_km <- haversine_distance_km(
      s$latitude[[1]], s$longitude[[1]],
      state$productive_latitude, state$productive_longitude
    )
    status_type <- if (identical(state$productive_location_source, "intake_approximation")) "warning" else "success"
    notice_box_ui(
      tagList(
        div(
          tags$strong(paste0(tx("productive_location"), ": ")),
          paste0(format(state$productive_latitude, digits = 8), ", ", format(state$productive_longitude, digits = 8))
        ),
        div(
          class = "subtle",
          paste0(tx("coordinate_source"), ": ", productive_source_label(state$productive_location_source))
        ),
        if (is.finite(distance_km)) div(class = "subtle", tx("intake_productive_distance", distance_km)) else NULL,
        div(class = "subtle", if (identical(state$productive_location_source, "polygon_centroid")) tx("polygon_calculation_note") else tx("productive_calculation_note"))
      ),
      status_type
    )
  })

  output$point_picker_status_ui <- renderUI({
    if (!isTRUE(state$point_pick_active)) return(NULL)
    div(
      class = "point-picker-active",
      div(tx("point_picker_active")),
      div(class = "subtle", tx("point_selected_crop", active_crop_name()))
    )
  })

  output$polygon_status_ui <- renderUI({
    if (isTRUE(state$polygon_draw_active)) {
      return(notice_box_ui(tagList(tx("polygon_drawing_active"), tags$br(), tags$span(class = "subtle", tx("polygon_crop_boundary"))), "info"))
    }
    if (is.null(state$polygon_metrics)) return(notice_box_ui(tx("polygon_help"), "info"))
    notice_box_ui(
      tagList(
        div(tx("polygon_recorded")),
        div(class = "subtle", tx("polygon_area_result", state$polygon_metrics$area_ha)),
        div(class = "subtle", tx("polygon_centroid_result", state$polygon_metrics$centroid_latitude, state$polygon_metrics$centroid_longitude)),
        div(class = "subtle", tx("polygon_crop_boundary"))
      ),
      "success"
    )
  })

  output$polygon_area_choice_ui <- renderUI({
    metrics <- state$polygon_metrics
    if (is.null(metrics)) return(NULL)
    declared <- suppressWarnings(as.numeric(input$area_ha %||% NA_real_))
    choices <- c("declared", "polygon")
    labels <- c(
      if (is.finite(declared)) tx("area_source_declared", declared) else tx("area_source_declared", 0),
      tx("area_source_polygon", metrics$area_ha)
    )
    div(
      class = "area-source-box",
      tags$strong(tx("area_source_question")),
      radioButtons(
        "area_source_choice", label = NULL,
        choices = stats::setNames(choices, labels),
        selected = state$area_source_choice %||% character(0)
      ),
      if (is.null(state$area_source_choice)) notice_box_ui(tx("area_choice_required"), "warning") else NULL
    )
  })

  observeEvent(input$area_source_choice, {
    choice <- as.character(input$area_source_choice %||% "")
    if (choice %in% c("declared", "polygon")) state$area_source_choice <- choice
  })

  observeEvent(input$activate_point_picker, {
    state$point_pick_active <- TRUE
    state$polygon_draw_active <- FALSE
  })

  observeEvent(input$activate_polygon_draw, {
    state$point_pick_active <- FALSE
    state$polygon_draw_active <- TRUE
    if (!is.null(state$productive_polygon)) {
      state$productive_polygon <- NULL
      state$polygon_metrics <- NULL
      state$polygon_crop_profile_id <- NULL
      state$area_source_choice <- NULL
      if (identical(state$productive_location_source, "polygon_centroid")) {
        state$productive_latitude <- NULL
        state$productive_longitude <- NULL
        state$productive_location_source <- NULL
      }
    }
    session$onFlushed(function() {
      session$sendCustomMessage("activatePolygonDraw", list())
    }, once = TRUE)
  })

  reset_polygon_state <- function(clear_location = FALSE) {
    state$productive_polygon <- NULL
    state$polygon_metrics <- NULL
    state$polygon_crop_profile_id <- NULL
    state$polygon_draw_active <- FALSE
    state$area_source_choice <- "declared"
    if (isTRUE(clear_location) && identical(state$productive_location_source, "polygon_centroid")) {
      state$productive_latitude <- NULL
      state$productive_longitude <- NULL
      state$productive_location_source <- NULL
    }
  }

  observeEvent(input$crop_profile_id, {
    new_crop <- as.character(input$crop_profile_id %||% "")
    if (!is.null(state$polygon_metrics) && nzchar(as.character(state$polygon_crop_profile_id %||% "")) &&
        !identical(new_crop, as.character(state$polygon_crop_profile_id))) {
      reset_polygon_state(TRUE)
      showNotification(tx("polygon_crop_change_reset"), type = "warning", duration = 9)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$use_intake, {
    s <- selected_system()
    reset_polygon_state(FALSE)
    state$productive_latitude <- s$latitude[[1]]
    state$productive_longitude <- s$longitude[[1]]
    state$productive_location_source <- "intake_approximation"
    state$point_pick_active <- FALSE
    if (is.finite(s$altitude_m[[1]])) updateNumericInput(session, "productive_elevation_m", value = s$altitude_m[[1]])
  })

  observeEvent(input$clear_productive, {
    state$productive_latitude <- NULL
    state$productive_longitude <- NULL
    state$productive_location_source <- NULL
    state$productive_polygon <- NULL
    state$polygon_metrics <- NULL
    state$polygon_crop_profile_id <- NULL
    state$polygon_draw_active <- FALSE
    state$area_source_choice <- "declared"
    state$point_pick_active <- FALSE
  })

  observeEvent(input$apply_manual_point, {
    lat <- as.numeric(input$manual_latitude); lon <- as.numeric(input$manual_longitude)
    if (!is.finite(lat) || !is.finite(lon) || lat < -90 || lat > 90 || lon < -180 || lon > 180) {
      showNotification(if (lang() == "en") "Invalid coordinates." else "Coordenadas inválidas.", type = "error")
      return()
    }
    reset_polygon_state(FALSE)
    state$productive_latitude <- lat
    state$productive_longitude <- lon
    state$productive_location_source <- "manual"
    state$point_pick_active <- FALSE
  })

  observeEvent(input$territory_map_click, {
    if (!isTRUE(state$point_pick_active)) return()
    click <- input$territory_map_click
    req(click$lat, click$lng)
    reset_polygon_state(FALSE)
    state$productive_latitude <- as.numeric(click$lat)
    state$productive_longitude <- as.numeric(click$lng)
    state$productive_location_source <- "map_click"
    state$point_pick_active <- FALSE
    updateNumericInput(session, "manual_latitude", value = click$lat)
    updateNumericInput(session, "manual_longitude", value = click$lng)
  })

  observeEvent(input$territory_map_marker_click, {
    marker <- input$territory_map_marker_click
    if (!is.null(marker$id) && marker$id %in% registry$public_id) {
      updateSelectInput(session, "system_public_id", selected = marker$id)
    }
  })

  apply_polygon_feature <- function(feature) {
    metrics <- tryCatch(polygon_metrics_from_feature(feature), error = function(e) e)
    if (inherits(metrics, "error") || is.null(metrics)) {
      showNotification(
        if (lang() == "en") "The polygon could not be interpreted. Draw at least three valid vertices." else "No se pudo interpretar el polígono. Dibuje al menos tres vértices válidos.",
        type = "error", duration = 8
      )
      return(FALSE)
    }
    state$productive_polygon <- feature
    state$polygon_metrics <- metrics
    state$polygon_crop_profile_id <- as.character(input$crop_profile_id %||% "papa_superchola")
    state$productive_latitude <- metrics$centroid_latitude
    state$productive_longitude <- metrics$centroid_longitude
    state$productive_location_source <- "polygon_centroid"
    state$area_source_choice <- NULL
    state$polygon_draw_active <- FALSE
    state$point_pick_active <- FALSE
    updateNumericInput(session, "manual_latitude", value = metrics$centroid_latitude)
    updateNumericInput(session, "manual_longitude", value = metrics$centroid_longitude)
    TRUE
  }

  observeEvent(input$territory_map_draw_new_feature, {
    apply_polygon_feature(input$territory_map_draw_new_feature)
  })
  observeEvent(input$territory_map_draw_edited_features, {
    edited <- input$territory_map_draw_edited_features
    if (!is.null(edited$features) && length(edited$features)) apply_polygon_feature(edited$features[[1]])
  })
  observeEvent(input$territory_map_draw_deleted_features, {
    reset_polygon_state(TRUE)
  })

  polygon_ring_df <- reactive({
    polygon_feature_ring_df(state$productive_polygon)
  })

  output$territory_map <- renderLeaflet({
    x <- registry_map_rows(filtered_registry())
    s <- registry_map_rows(selected_system())
    focus_rows <- if (identical(normalise_registry_filter(input$canton_filter), "__ALL__")) {
      registry
    } else {
      filter_system_registry(registry, canton = input$canton_filter, parish = "__ALL__")
    }
    bounds <- registry_map_bounds(focus_rows)
    shiny::validate(shiny::need(
      nrow(x) > 0L,
      if (lang() == "en") "No georeferenced systems match the active filters." else "No existen sistemas georreferenciados para los filtros activos."
    ))
    other <- x[x$public_id != s$public_id[[1]], , drop = FALSE]
    legend_html <- paste0(
      "<div class='map-legend'><div class='map-legend-title'>", htmlEscape(tx("map_legend_title")), "</div>",
      "<div><span class='legend-symbol legend-other'></span>", htmlEscape(tx("map_legend_other")), "</div>",
      "<div><span class='legend-symbol legend-intake'></span>", htmlEscape(tx("map_legend_intake")), "</div>",
      "<div><span class='legend-symbol legend-productive'></span>", htmlEscape(tx("map_legend_productive")), "</div>",
      "<div><span class='legend-symbol legend-polygon'></span>", htmlEscape(tx("map_legend_polygon")), "</div></div>"
    )
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(noWrap = TRUE)) |>
      fitBounds(lng1 = bounds[["lng1"]], lat1 = bounds[["lat1"]], lng2 = bounds[["lng2"]], lat2 = bounds[["lat2"]]) |>
      addControl(html = legend_html, position = "bottomright")
    if (nrow(other)) {
      m <- m |> addCircleMarkers(
        data = other, lng = ~longitude, lat = ~latitude, layerId = ~public_id,
        radius = 4, stroke = TRUE, weight = 1, color = "#475569",
        fillColor = "#94a3b8", fillOpacity = .58,
        popup = ~paste0("<strong>", htmlEscape(public_id), "</strong><br>", htmlEscape(system_name),
                        "<br><span style='font-size:12px'>", htmlEscape(canton_display),
                        ifelse(nzchar(parish_display), paste0(" / ", htmlEscape(parish_display)), ""), "</span>")
      )
    }
    m <- m |> addCircleMarkers(
      data = s, lng = ~longitude, lat = ~latitude, layerId = ~public_id,
      radius = 10, stroke = TRUE, weight = 3, color = "#17365d",
      fillColor = "#2563eb", fillOpacity = .95,
      popup = paste0("<strong>", tx("map_intake_popup"), "</strong><br>", htmlEscape(s$public_id[[1]]), " — ", htmlEscape(s$system_name[[1]]))
    )
    if (!is.null(state$productive_latitude) && !is.null(state$productive_longitude)) {
      distance_km <- haversine_distance_km(
        s$latitude[[1]], s$longitude[[1]],
        state$productive_latitude, state$productive_longitude
      )
      m <- m |>
        addPolylines(
          lng = c(s$longitude[[1]], state$productive_longitude),
          lat = c(s$latitude[[1]], state$productive_latitude),
          color = "#64748b", weight = 2, opacity = .8, dashArray = "6,6",
          popup = if (is.finite(distance_km)) tx("intake_productive_distance", distance_km) else NULL
        ) |>
        addMarkers(
          lng = state$productive_longitude, lat = state$productive_latitude,
          popup = paste0("<strong>", tx("map_productive_popup"), "</strong><br>", htmlEscape(productive_source_label(state$productive_location_source))),
          icon = makeAwesomeIcon(icon = "leaf", library = "fa", markerColor = "green")
        )
    }
    polygon_df <- polygon_ring_df()
    if (!is.null(polygon_df) && nrow(polygon_df) >= 3L) {
      m <- m |> addPolygons(
        lng = polygon_df$lng, lat = polygon_df$lat,
        group = "productive_polygon", color = "#ea580c", weight = 3,
        fillColor = "#f97316", fillOpacity = .18,
        popup = tx("polygon_recorded")
      )
    }
    if (isTRUE(state$polygon_draw_active) || isTRUE(input$technical_view)) {
      m <- m |> addDrawToolbar(
        targetGroup = "productive_polygon",
        polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE,
        markerOptions = FALSE, circleMarkerOptions = FALSE,
        polygonOptions = drawPolygonOptions(showArea = TRUE),
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      )
    }
    m
  })

  output$weather_mode_note_ui <- renderUI({
    if (identical(input$weather_mode, "offline_fixture")) return(notice_box_ui(tx("weather_warning_offline"), "warning"))
    if (identical(input$weather_mode, "archived_fixture")) return(notice_box_ui(tx("weather_warning_archived"), "warning"))
    if (identical(input$weather_mode, "live_openmeteo") || identical(input$soil_mode, "live_soilgrids")) return(notice_box_ui(tx("live_data_note"), "info"))
    NULL
  })

  observeEvent(input$initial_mode, {
    mode <- input$initial_mode
    if (mode == "field_capacity") {
      updateNumericInput(session, "initial_value", value = 0, min = 0, max = 0, step = 0.001)
    } else if (mode %in% c("fraction_raw", "fraction_taw")) {
      val <- if (mode == "fraction_raw") 0.90 else 0.25
      updateNumericInput(session, "initial_value", value = val, min = 0, max = if (mode == "fraction_raw") 2 else 1, step = 0.05)
    } else if (mode == "measured_theta") {
      updateNumericInput(session, "initial_value", value = 0.25, min = 0, max = 0.8, step = 0.001)
    } else {
      updateNumericInput(session, "initial_value", value = 20, min = 0, max = 10000, step = 1)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$reset_scenario, {
    updateSelectInput(session, "crop_profile_id", selected = "papa_superchola")
    updateRadioButtons(session, "crop_age_mode", selected = "crop_age")
    updateNumericInput(session, "crop_age_days", value = 60)
    updateNumericInput(session, "area_ha", value = 0.25)
    updateNumericInput(session, "delivery_efficiency", value = 0.75)
    updateDateInput(session, "current_turn_date", value = Sys.Date() + 1)
    updateDateInput(session, "next_turn_date", value = Sys.Date() + 8)
    updateNumericInput(session, "turn_hours", value = 6)
    updateNumericInput(session, "flow_lps", value = 2)
    updateSelectInput(session, "flow_source", selected = "declared")
    updateCheckboxInput(session, "flow_verified", value = FALSE)
    updateCheckboxInput(session, "flow_context_documented", value = FALSE)
    updateCheckboxInput(session, "turn_schedule_verified", value = FALSE)
    updateSelectInput(session, "recent_irrigation_history_status", selected = "unavailable")
    updateSelectInput(session, "weather_mode", selected = "offline_fixture")
    updateSelectInput(session, "soil_mode", selected = if (selected_system()$public_id[[1]] == configured_demo_system_id) "archived_santul" else "fao_reference")
    updateSelectInput(session, "initial_condition_simple", selected = "dry")
    updateSelectInput(session, "initial_mode", selected = "fraction_raw")
    updateNumericInput(session, "initial_value", value = 0.90)
    updateCheckboxInput(session, "initial_water_verified", value = FALSE)
    updateSelectInput(session, "subsoil_mode", selected = "field_capacity")
    updateCheckboxInput(session, "subsoil_verified", value = FALSE)
    updateNumericInput(session, "runoff_fraction", value = 0)
    if (is.null(state$polygon_metrics)) state$area_source_choice <- "declared"
    state$result <- NULL
    state$error <- NULL
  })

  effective_area_ha <- reactive({
    declared <- suppressWarnings(as.numeric(input$area_ha %||% NA_real_))
    if (!is.null(state$polygon_metrics) && identical(state$area_source_choice, "polygon")) {
      return(as.numeric(state$polygon_metrics$area_ha))
    }
    declared
  })

  collect_inputs <- reactive({
    polygon_json <- if (is.null(state$productive_polygon)) "" else jsonlite::toJSON(state$productive_polygon, auto_unbox = TRUE)
    simple_initial <- input$initial_condition_simple %||% "dry"
    if (identical(simple_initial, "technical")) {
      initial_pair <- list(
        mode = input$initial_mode %||% "fraction_raw",
        value = input$initial_value %||% 0.90,
        verified = isTRUE(input$initial_water_verified)
      )
    } else {
      preset <- initial_state_presets[initial_state_presets$preset_id == simple_initial, , drop = FALSE]
      if (nrow(preset) != 1L) preset <- initial_state_presets[initial_state_presets$preset_id == "dry", , drop = FALSE]
      initial_pair <- list(
        mode = preset$initial_mode[[1]],
        value = preset$initial_value[[1]],
        verified = isTRUE(preset$verified[[1]])
      )
    }
    list(
      system_public_id = state$selected_system_id,
      productive_latitude = state$productive_latitude,
      productive_longitude = state$productive_longitude,
      productive_elevation_m = input$productive_elevation_m,
      productive_location_source = state$productive_location_source,
      productive_polygon_geojson = polygon_json,
      polygon_area_ha = if (is.null(state$polygon_metrics)) NA_real_ else state$polygon_metrics$area_ha,
      polygon_centroid_latitude = if (is.null(state$polygon_metrics)) NA_real_ else state$polygon_metrics$centroid_latitude,
      polygon_centroid_longitude = if (is.null(state$polygon_metrics)) NA_real_ else state$polygon_metrics$centroid_longitude,
      area_source = state$area_source_choice %||% "declared",
      declared_area_ha = input$area_ha,
      crop_area_scope = "single_crop_homogeneous_zone",
      crop_profile_id = input$crop_profile_id,
      crop_age_mode = input$crop_age_mode,
      planting_date = as.character(input$planting_date),
      crop_age_days = input$crop_age_days,
      area_ha = effective_area_ha(),
      delivery_efficiency = input$delivery_efficiency,
      current_turn_date = as.character(input$current_turn_date),
      next_turn_date = as.character(input$next_turn_date),
      turn_hours = input$turn_hours,
      flow_lps = input$flow_lps,
      flow_source = input$flow_source,
      flow_verified = isTRUE(input$flow_verified),
      flow_measurement_date = if (!is.null(input$flow_measurement_date)) as.character(input$flow_measurement_date) else "",
      flow_measurement_location = input$flow_measurement_location %||% "",
      flow_measurement_method_detail = input$flow_measurement_method_detail %||% "",
      flow_repetition_count = input$flow_repetition_count %||% 0,
      flow_context_documented = isTRUE(input$flow_context_documented),
      turn_schedule_verified = isTRUE(input$turn_schedule_verified),
      recent_irrigation_history_status = input$recent_irrigation_history_status %||% "unavailable",
      weather_mode = input$weather_mode,
      weather_upload_path = if (!is.null(input$weather_file)) input$weather_file$datapath else NULL,
      soil_mode = input$soil_mode,
      reference_soil_id = input$reference_soil_id %||% "medium_silt",
      local_theta_fc = input$local_theta_fc %||% NA_real_,
      local_theta_wp = input$local_theta_wp %||% NA_real_,
      local_soil_verified = isTRUE(input$local_soil_verified),
      initial_mode = initial_pair$mode,
      initial_value = initial_pair$value,
      initial_water_verified = initial_pair$verified,
      subsoil_mode = input$subsoil_mode %||% "field_capacity",
      subsoil_theta_value = input$subsoil_theta_value %||% NA_real_,
      subsoil_verified = isTRUE(input$subsoil_verified),
      runoff_fraction = if (isTRUE(input$technical_view)) input$runoff_fraction %||% 0 else 0
    )
  })

  run_plan <- function(success_step = "results") {
    if (is.null(state$productive_latitude) || is.null(state$productive_longitude)) {
      showNotification(tx("productive_location_missing"), type = "error", duration = 8)
      updateRadioButtons(session, "step", selected = "territory")
      return(FALSE)
    }
    if (!is.null(state$polygon_metrics) && is.null(state$area_source_choice)) {
      showNotification(tx("area_choice_required"), type = "error", duration = 8)
      updateRadioButtons(session, "step", selected = "territory")
      return(FALSE)
    }
    if (!is.finite(effective_area_ha()) || effective_area_ha() <= 0) {
      showNotification(tx("area_crop_note"), type = "error", duration = 8)
      updateRadioButtons(session, "step", selected = "scenario")
      return(FALSE)
    }
    state$error <- NULL
    result <- tryCatch(
      withProgress(message = tx("calculating"), value = 0.15, {
        incProgress(0.15)
        z <- compute_riegoazuay_plan(collect_inputs(), registry, app_config)
        incProgress(0.70)
        z
      }),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      state$error <- conditionMessage(result)
      state$result <- NULL
      showNotification(paste(tx("error_prefix"), conditionMessage(result)), type = "error", duration = NULL)
      return(FALSE)
    }
    state$result <- result
    state$error <- NULL
    updateRadioButtons(session, "step", selected = success_step)
    showNotification(
      if (lang() == "en") paste0("Plan calculated. Evidence level: ", result$readiness$level) else paste0("Plan calculado. Nivel de evidencia: ", result$readiness$level),
      type = "message", duration = 6
    )
    TRUE
  }

  observeEvent(input$calculate_plan, {
    run_plan("results")
  })

  step_order <- c("territory", "scenario", "evidence", "results", "traceability")

  observeEvent(input$nav_back, {
    current <- input$step %||% "territory"
    idx <- match(current, step_order)
    if (is.na(idx) || idx <= 1L) {
      showNotification(tx("first_step_notice"), type = "message", duration = 4)
      return()
    }
    updateRadioButtons(session, "step", selected = step_order[[idx - 1L]])
  })

  observeEvent(input$nav_next, {
    current <- input$step %||% "territory"
    if (identical(current, "territory")) {
      if (is.null(state$productive_latitude) || is.null(state$productive_longitude)) {
        showNotification(tx("productive_location_missing"), type = "error", duration = 8)
        return()
      }
      if (!is.null(state$polygon_metrics) && is.null(state$area_source_choice)) {
        showNotification(tx("area_choice_required"), type = "error", duration = 8)
        return()
      }
      updateRadioButtons(session, "step", selected = "scenario")
      return()
    }
    if (identical(current, "scenario")) {
      run_plan("evidence")
      return()
    }
    if (identical(current, "evidence")) {
      if (is.null(state$result)) {
        showNotification(tx("result_required"), type = "error", duration = 7)
        updateRadioButtons(session, "step", selected = "scenario")
        return()
      }
      updateRadioButtons(session, "step", selected = "results")
      return()
    }
    if (identical(current, "results")) {
      if (is.null(state$result)) {
        showNotification(tx("result_required"), type = "error", duration = 7)
        updateRadioButtons(session, "step", selected = "scenario")
        return()
      }
      updateRadioButtons(session, "step", selected = "traceability")
      return()
    }
    showNotification(tx("last_step_notice"), type = "message", duration = 4)
  })

  observeEvent(list(input$step, input$language), {
    l <- lang()
    current <- input$step %||% "territory"
    next_label <- if (identical(current, "scenario")) tr_app("next_calculate", l) else tr_app("next_step", l)
    updateActionButton(session, "nav_back", label = tr_app("back", l))
    updateActionButton(session, "nav_next", label = next_label)
  }, ignoreInit = FALSE)

  output$readiness_ui <- renderUI({
    if (is.null(state$result)) {
      return(notice_box_ui(if (lang() == "en") "Calculate a scenario to assign the active evidence level." else "Calcule un escenario para asignar el nivel de evidencia activo.", "info"))
    }
    level <- state$result$readiness$level
    label <- readiness_label_app(level, evidence_levels, lang())
    title <- if (identical(level, "E2")) tx("provisional_plain") else label
    explanation <- if (identical(level, "E2")) tx("provisional_explanation") else {
      if (lang() == "en") paste0("The active evidence level authorises: ", label, ".") else paste0("El nivel de evidencia activo autoriza: ", label, ".")
    }
    div(
      class = "evidence-hero",
      h2(title),
      p(explanation),
      div(class = "evidence-code", tx("readiness_internal_code", level))
    )
  })
  output$readiness_boundary_ui <- renderUI(notice_box_ui(tx("readiness_boundary"), "warning"))

  output$simple_evidence_summary_ui <- renderUI({
    if (is.null(state$result)) {
      return(notice_box_ui(if (lang() == "en") "Calculate the plan to identify the information that still needs verification." else "Calcule el plan para identificar qué información falta verificar.", "info"))
    }
    ptab <- state$result$provenance
    critical <- ptab[!as.logical(ptab$verified) | ptab$provenance_class %in% c("MISSING", "SYNTHETIC_TEST", "PUBLIC_MODELLED", "USER_DECLARED"), , drop = FALSE]
    ids <- unique(as.character(critical$input_group))
    actions <- character(0)
    if ("productive_location" %in% ids || identical(state$result$inputs$productive_location_source, "intake_approximation")) {
      actions <- c(actions, tx("priority_action_location"))
    }
    if ("active_flow" %in% ids) actions <- c(actions, tx("priority_action_flow"))
    if ("turn_schedule" %in% ids) actions <- c(actions, tx("priority_action_turn"))
    if (any(c("weather", "soil") %in% ids)) actions <- c(actions, tx("priority_action_weather_soil"))
    if (any(c("initial_root_zone_water", "recent_irrigation_history", "subsoil_water") %in% ids)) {
      actions <- c(
        actions,
        if (lang() == "en") "Record recent irrigation and, where possible, measure the current soil-water state." else "Registre el riego reciente y, cuando sea posible, mida el estado hídrico actual del suelo."
      )
    }
    actions <- unique(actions)
    if (!length(actions)) {
      return(notice_box_ui(if (lang() == "en") "No decision-critical unverified input was detected." else "No se detectó un insumo crítico sin verificar.", "success"))
    }
    tagList(
      h4(tx("priority_actions_title")),
      tags$ol(class = "simple-warning-list", lapply(actions, tags$li))
    )
  })

  output$evidence_framework_table <- renderDT({
    x <- evidence_levels
    if (lang() == "en") {
      names(x)[names(x) == "label_en"] <- "Interpretation"
      names(x)[names(x) == "requirements_en"] <- "Minimum evidence condition"
      names(x)[names(x) == "authorized_output_en"] <- "Authorised output"
      x <- x[, c("level", "Interpretation", "Minimum evidence condition", "Authorised output"), drop = FALSE]
    } else {
      names(x)[names(x) == "label_es"] <- "Interpretación"
      names(x)[names(x) == "requirements_es"] <- "Condición mínima de evidencia"
      names(x)[names(x) == "authorized_output_es"] <- "Salida autorizada"
      x <- x[, c("level", "Interpretación", "Condición mínima de evidencia", "Salida autorizada"), drop = FALSE]
    }
    datatable(x, rownames = FALSE, options = list(pageLength = 5, dom = "t", scrollX = TRUE))
  })

  output$provenance_empty_ui <- renderUI({
    if (is.null(state$result)) notice_box_ui(if (lang() == "en") "No active provenance summary is available until a scenario is calculated." else "El resumen de procedencia se genera al calcular un escenario.", "info")
  })

  output$simple_provenance_ui <- renderUI({
    req(state$result)
    x <- state$result$provenance
    core_ids <- c("productive_location", "productive_area", "weather", "soil", "active_flow", "turn_schedule")
    labels_es <- c(
      productive_location = "Ubicación del cultivo",
      productive_area = "Área del cultivo",
      weather = "Meteorología",
      soil = "Suelo",
      active_flow = "Caudal",
      turn_schedule = "Calendario de turnos"
    )
    labels_en <- c(
      productive_location = "Crop location",
      productive_area = "Crop area",
      weather = "Weather",
      soil = "Soil",
      active_flow = "Flow",
      turn_schedule = "Turn schedule"
    )
    labels <- if (lang() == "en") labels_en else labels_es
    rows <- lapply(core_ids, function(id) {
      z <- x[x$input_group == id, , drop = FALSE]
      if (!nrow(z)) return(NULL)
      z <- z[1, , drop = FALSE]
      source_label <- provenance_label_app(z$provenance_class[[1]], provenance_classes, lang())
      div(
        class = "source-summary-row",
        div(class = "source-summary-label", labels[[id]]),
        div(
          class = "source-summary-value",
          render_value_tag(source_label, source_class_css(z$provenance_class[[1]]))
        )
      )
    })
    rows <- Filter(Negate(is.null), rows)
    tagList(
      p(tx("provenance_simple_intro")),
      div(class = "source-summary-grid", rows),
      p(class = "subtle", tx("provenance_technical_hint"))
    )
  })

  output$provenance_table <- renderDT({
    req(state$result)
    x <- state$result$provenance
    x$provenance_label <- vapply(x$provenance_class, provenance_label_app, character(1), provenance_classes = provenance_classes, language = lang())
    x <- x[, c("input_group", "active_value", "provenance_label", "verified", "calculation_role")]
    datatable(x, rownames = FALSE, filter = "top", options = list(pageLength = 14, scrollX = TRUE))
  })

  output$no_result_ui <- renderUI({
    if (!is.null(state$result)) return(NULL)
    text <- if (!is.null(state$error)) paste(tx("error_prefix"), state$error) else tx("no_result")
    notice_box_ui(text, if (!is.null(state$error)) "danger" else "info")
  })

  output$action_summary_ui <- renderUI({
    req(state$result)
    r <- state$result
    csum <- r$central_summary[1, , drop = FALSE]
    events <- r$central_events
    turn_date <- if (nrow(events)) as.Date(events$date[[1]]) else as.Date(r$inputs$current_turn_date)
    valid_until <- max(as.Date(r$central_daily$date))
    required <- as.numeric(csum$required_volume_m3[[1]])
    deliverable <- as.numeric(csum$deliverable_volume_m3[[1]])
    unmet <- as.numeric(csum$unmet_volume_m3[[1]])
    duration <- if (nrow(events)) as.numeric(events$deliverable_hours[[1]]) else as.numeric(csum$required_hours[[1]])
    if (required <= 1e-9) {
      primary <- if (lang() == "en") {
        paste0("Do not irrigate at the turn on ", format(turn_date, "%d/%m/%Y"), ".")
      } else {
        paste0("No riegue en el turno del ", format(turn_date, "%d/%m/%Y"), ".")
      }
      warning_class <- ""
    } else if (unmet > 1e-9) {
      primary <- if (lang() == "en") {
        paste0("At the turn on ", format(turn_date, "%d/%m/%Y"), ", apply up to ", format_metric(deliverable, 2, " m³"),
               " (", format_metric(deliverable * 1000, 0, " L"), ") for ", format_duration_hm_app(duration, "en"),
               ". The turn leaves ", format_metric(unmet, 2, " m³"), " unmet.")
      } else {
        paste0("En el turno del ", format(turn_date, "%d/%m/%Y"), ", aplique hasta ", format_metric(deliverable, 2, " m³"),
               " (", format_metric(deliverable * 1000, 0, " L"), ") durante ", format_duration_hm_app(duration, "es"),
               ". El turno deja ", format_metric(unmet, 2, " m³"), " sin atender.")
      }
      warning_class <- "warning"
    } else {
      primary <- if (lang() == "en") {
        paste0("At the turn on ", format(turn_date, "%d/%m/%Y"), ", apply ", format_metric(deliverable, 2, " m³"),
               " (", format_metric(deliverable * 1000, 0, " L"), ") for ", format_duration_hm_app(duration, "en"), ".")
      } else {
        paste0("En el turno del ", format(turn_date, "%d/%m/%Y"), ", aplique ", format_metric(deliverable, 2, " m³"),
               " (", format_metric(deliverable * 1000, 0, " L"), ") durante ", format_duration_hm_app(duration, "es"), ".")
      }
      warning_class <- ""
    }
    crop_label <- if (lang() == "en") r$profile$crop_name_en[[1]] else r$profile$crop_name_es[[1]]
    area_source_label <- if (identical(r$inputs$area_source, "polygon")) tx("area_source_polygon_short") else tx("area_source_declared_short")
    div(
      class = paste("action-summary", warning_class),
      h2(tx("next_turn_instruction")),
      div(
        class = "subtle",
        if (lang() == "en") {
          paste0("Plan for ", crop_label, " on ", format_metric(r$inputs$area_ha, 4, " ha"), " (", area_source_label, ").")
        } else {
          paste0("Plan para ", crop_label, " en ", format_metric(r$inputs$area_ha, 4, " ha"), " (", area_source_label, ").")
        }
      ),
      div(class = "primary-action", primary),
      div(class = "subtle", tx("plan_valid_until", format(valid_until, "%d/%m/%Y")))
    )
  })

  output$daily_plan_note_ui <- renderUI(notice_box_ui(tx("seven_day_note"), "info"))

  output$daily_plan_ui <- renderUI({
    req(state$result)
    daily_action_cards_ui(build_daily_action_plan_table(state$result), lang())
  })

  output$priority_warnings_ui <- renderUI({
    req(state$result)
    wt <- state$result$warnings_table
    if (!nrow(wt)) return(notice_box_ui(if (lang() == "en") "No warning was generated." else "No se generaron advertencias.", "success"))
    priority_codes <- c(
      "FLOW_UNVERIFIED", "TURN_UNVERIFIED", "WEATHER_SYNTHETIC", "WEATHER_ARCHIVED",
      "WEATHER_PUBLIC_MODEL", "SOIL_PUBLIC_MODEL", "SOIL_REFERENCE", "INITIAL_WATER_UNVERIFIED",
      "HISTORY_MISSING", "READINESS_E2"
    )
    rank <- match(wt$warning_code, priority_codes)
    rank[is.na(rank)] <- length(priority_codes) + seq_len(sum(is.na(rank)))
    wt <- wt[order(rank), , drop = FALSE]
    messages <- if (lang() == "en") wt$message_en else wt$message_es
    messages <- messages[nzchar(trimws(messages))]
    priority <- head(messages, 3L)
    remaining <- if (length(messages) > 3L) messages[-seq_len(3L)] else character(0)
    tagList(
      tags$ul(class = "simple-warning-list", lapply(priority, tags$li)),
      if (length(remaining)) tags$details(
        class = "details-box",
        tags$summary(tx("all_warnings")),
        tags$ul(class = "warning-list", lapply(remaining, tags$li))
      ) else NULL
    )
  })

  output$result_metrics_ui <- renderUI({
    req(state$result)
    r <- state$result
    csum <- r$central_summary[1, , drop = FALSE]
    env <- r$envelope[1, , drop = FALSE]
    stage <- if (lang() == "en") r$crop_stage_en else r$crop_stage_es
    capacity_note <- if (csum$required_volume_m3[[1]] <= 1e-9) tx("no_irrigation") else if (csum$unmet_volume_m3[[1]] > 1e-9) tx("capacity_limited") else tx("capacity_ok")
    tagList(
      div(
        class = "metric-grid",
        metric_card_ui(
          tx("volume_required"), format_metric(csum$required_volume_m3, 2, " m³"),
          paste0(format_metric(csum$required_liters, 0, " L"), " · ", tx("sensitivity_hidden_simple"))
        ),
        metric_card_ui(tx("required_time"), format_metric(csum$required_hours, 2, " h"), paste0(tx("turn_hours"), ": ", format_metric(csum$hours_available, 2, " h"))),
        metric_card_ui(tx("deliverable_volume"), format_metric(csum$deliverable_volume_m3, 2, " m³"), paste0(tx("unmet_volume"), ": ", format_metric(csum$unmet_volume_m3, 2, " m³"))),
        metric_card_ui(
          tx("readiness"),
          if (identical(r$readiness$level, "E2")) tx("provisional_plain") else readiness_label_app(r$readiness$level, evidence_levels, lang()),
          paste0(r$readiness$level, " · ", tx("crop_stage"), ": ", stage)
        )
      ),
      notice_box_ui(capacity_note, if (csum$unmet_volume_m3[[1]] > 1e-9) "warning" else "success"),
      notice_box_ui(tx("provisional"), "info")
    )
  })

  output$warnings_ui <- renderUI({
    req(state$result)
    messages <- if (lang() == "en") state$result$warnings_en else state$result$warnings_es
    tags$ul(class = "warning-list", lapply(messages, tags$li))
  })

  output$scenario_range_plot <- renderPlot({
    req(state$result)
    x <- state$result$scenario_summary
    x$weather_label <- vapply(x$weather_scenario, scenario_label_app, character(1), language = lang())
    x$soil_label <- x$soil_scenario
    ggplot(x, aes(x = weather_label, y = required_volume_m3, group = soil_label, shape = soil_label)) +
      geom_line(aes(linetype = soil_label), linewidth = .7) +
      geom_point(size = 3) +
      geom_hline(yintercept = state$result$envelope$central_turn_capacity_m3[[1]], linetype = "dashed") +
      labs(
        x = if (lang() == "en") "Weather scenario" else "Escenario meteorológico",
        y = if (lang() == "en") "Required gross volume (m³)" else "Volumen bruto requerido (m³)",
        shape = if (lang() == "en") "Soil" else "Suelo",
        linetype = if (lang() == "en") "Soil" else "Suelo",
        caption = if (lang() == "en") "Dashed line: central turn capacity." else "Línea discontinua: capacidad del turno central."
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$daily_balance_plot <- renderPlot({
    req(state$result)
    x <- state$result$central_daily
    ggplot(x, aes(x = as.Date(date))) +
      geom_line(aes(y = depletion_end_mm), linewidth = .9) +
      geom_line(aes(y = raw_mm), linetype = "dashed", linewidth = .8) +
      geom_point(data = x[x$irrigation_gross_mm_day > 0, , drop = FALSE], aes(y = depletion_end_mm), size = 3) +
      labs(
        x = NULL,
        y = if (lang() == "en") "Root-zone depletion (mm)" else "Agotamiento de la zona radicular (mm)",
        caption = if (lang() == "en") "Solid: end-of-day depletion; dashed: readily available water threshold." else "Continua: agotamiento al final del día; discontinua: umbral de agua fácilmente aprovechable."
      ) + theme_minimal(base_size = 12)
  })

  output$scenario_table <- renderDT({
    req(state$result)
    datatable(state$result$scenario_summary, rownames = FALSE, filter = "top", options = list(pageLength = 12, scrollX = TRUE))
  })
  output$daily_balance_table <- renderDT({
    req(state$result)
    datatable(state$result$central_daily, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  output$event_table <- renderDT({
    req(state$result)
    datatable(state$result$central_events, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$soil_table <- renderDT({
    req(state$result)
    datatable(state$result$soil_layers, rownames = FALSE, filter = "top", options = list(pageLength = 12, scrollX = TRUE))
  })
  output$diagnostics_table <- renderDT({
    req(state$result)
    datatable(state$result$diagnostics, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$release_text <- renderText(paste0(app_config$release_tag, ". ", tx("release_pending")))
  output$traceability_note_ui <- renderUI(notice_box_ui(tx("traceability_note"), "info"))
  output$sdg_boundary_ui <- renderUI(notice_box_ui(tx("sdg_boundary"), "warning"))

  output$sdg_table <- renderDT({
    x <- sdg_layer
    if (lang() == "en") {
      x <- x[, c("target", "function_en", "interpretive_boundary_en")]
      names(x) <- c("Target", "Platform function", "Interpretive boundary")
    } else {
      x <- x[, c("target", "function_es", "interpretive_boundary_es")]
      names(x) <- c("Meta", "Función de la plataforma", "Límite interpretativo")
    }
    datatable(x, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$download_weather_template <- downloadHandler(
    filename = function() "RiegoAzuay_weather_template.csv",
    content = function(file) file.copy(file.path("data", "templates", "manual_weather_template.csv"), file, overwrite = TRUE)
  )
  output$download_user_excel <- downloadHandler(
    filename = function() {
      req(state$result)
      paste0("RiegoAzuay_", state$result$run_id, "_plan_", toupper(lang()), ".xlsx")
    },
    content = function(file) {
      req(state$result)
      write_user_xlsx_export(state$result, file, lang())
    }
  )
  output$download_daily_plan <- downloadHandler(
    filename = function() paste0("RiegoAzuay_", state$result$run_id, "_daily_plan_ES_EN.csv"),
    content = function(file) { req(state$result); write_csv_utf8(build_daily_action_plan_table(state$result), file) }
  )
  output$download_results_csv <- downloadHandler(
    filename = function() paste0("RiegoAzuay_", state$result$run_id, "_scenario_summary.csv"),
    content = function(file) { req(state$result); write_result_csv_export(state$result, file) }
  )
  output$download_report <- downloadHandler(
    filename = function() paste0("RiegoAzuay_", state$result$run_id, "_report_ES_EN.html"),
    content = function(file) { req(state$result); write_bilingual_html_report(state$result, file) }
  )
  output$download_rds <- downloadHandler(
    filename = function() paste0("RiegoAzuay_", state$result$run_id, ".rds"),
    content = function(file) { req(state$result); saveRDS(state$result, file) }
  )
  output$download_bundle <- downloadHandler(
    filename = function() paste0("RiegoAzuay_", state$result$run_id, "_reproducibility_bundle.zip"),
    content = function(file) { req(state$result); build_reproducibility_export(state$result, file) }
  )
}

shinyApp(ui, server)
