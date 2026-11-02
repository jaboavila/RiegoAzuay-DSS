if (!file.exists("app.R") || !dir.exists("R")) {
  stop(
    "Open RiegoAzuay-DSS.Rproj or set the working directory to the extracted project root before running this script.",
    call. = FALSE
  )
}

cat("============================================================\n")
cat("RIEGOAZUAY DSS v1.0.0-rc6\n")
cat("LIVE PUBLIC-SOURCE RELEASE GATE\n")
cat("============================================================\n")
cat("This workflow does not rerun the retrospective experiment and does not launch the Shiny application.\n")
cat("It audits the configured Open-Meteo and SoilGrids routes, archives source receipts, and creates a review bundle.\n\n")

source("00_syntax_gate.R", encoding = "UTF-8")
source("00_install_packages.R", encoding = "UTF-8")
previous_live_gate_mode <- Sys.getenv("RIEGOAZUAY_LIVE_GATE", unset = "")
Sys.setenv(RIEGOAZUAY_LIVE_GATE = "1")
source("01_static_preflight.R", encoding = "UTF-8")
if (nzchar(previous_live_gate_mode)) {
  Sys.setenv(RIEGOAZUAY_LIVE_GATE = previous_live_gate_mode)
} else {
  Sys.unsetenv("RIEGOAZUAY_LIVE_GATE")
}
rm(previous_live_gate_mode)
source("07_run_live_source_audit.R", encoding = "UTF-8")
