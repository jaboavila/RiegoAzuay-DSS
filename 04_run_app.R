if (!file.exists("app.R") || !dir.exists("R")) {
  stop("Open RiegoAzuay-DSS.Rproj or set the working directory to the extracted project root before running this script.", call. = FALSE)
}

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop('Package "shiny" is unavailable. Run source("00_install_packages.R") first.', call. = FALSE)
}
cat("Launching RiegoAzuay DSS v1.0.0.\n")
cat("Original software code is licensed under the MIT License.\n")
shiny::runApp(".", launch.browser = TRUE)
