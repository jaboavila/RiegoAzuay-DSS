if (!file.exists("app.R") || !dir.exists("R")) {
  stop("Open RiegoAzuay-DSS.Rproj or set the working directory to the extracted project root before running this script.", call. = FALSE)
}

cat("============================================================\n")
cat("RIEGOAZUAY DSS v1.0.0\n")
cat("PUBLIC SOFTWARE RELEASE\n")
cat("============================================================\n")
cat("This workflow validates the integrated application with offline scientific and integration tests.\n")
cat("It does not rerun the closed 2304-scenario retrospective experiment.\n\n")
source("00_syntax_gate.R", encoding = "UTF-8")
source("00_install_packages.R", encoding = "UTF-8")
source("01_static_preflight.R", encoding = "UTF-8")
source("02_run_scientific_tests.R", encoding = "UTF-8")
source("03_run_integration_smoke_tests.R", encoding = "UTF-8")
cat("\nAll public-release checks completed. The browser application will now open.\n")
source("04_run_app.R", encoding = "UTF-8")
