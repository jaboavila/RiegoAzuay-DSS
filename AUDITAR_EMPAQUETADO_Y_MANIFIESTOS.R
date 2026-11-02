if (!file.exists("app.R") || !dir.exists("R")) {
  stop(
    "Open RiegoAzuay-DSS.Rproj or set the working directory to the extracted project root before running this script.",
    call. = FALSE
  )
}

cat("============================================================\n")
cat("RIEGOAZUAY DSS v1.0.0-rc6\n")
cat("PACKAGING AND PATH-RESOLVED MANIFEST GATE\n")
cat("============================================================\n")
cat("This workflow does not access external APIs, rerun the retrospective experiment, or launch Shiny.\n")
cat("It validates corrected path-resolved manifests and the preserved alpha15 live-source evidence.\n\n")

source("00_syntax_gate.R", encoding = "UTF-8")
source("00_install_packages.R", encoding = "UTF-8")
source("08_run_packaging_manifest_gate.R", encoding = "UTF-8")
