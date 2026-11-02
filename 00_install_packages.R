runtime_register_path <- file.path("config", "runtime_packages_v1.csv")
if (!file.exists(runtime_register_path)) {
  stop("Runtime package register is missing: ", runtime_register_path, call. = FALSE)
}
runtime_register <- utils::read.csv(runtime_register_path, stringsAsFactors = FALSE, check.names = FALSE)
required_packages <- unique(trimws(runtime_register$package))
required_packages <- required_packages[nzchar(required_packages)]

installed <- rownames(utils::installed.packages())
missing <- setdiff(required_packages, installed)
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  utils::install.packages(missing, repos = "https://cloud.r-project.org", dependencies = TRUE)
}

still_missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing)) {
  stop(
    "The following packages remain unavailable: ", paste(still_missing, collapse = ", "),
    ". Review the installation messages before continuing.", call. = FALSE
  )
}

cat("Package check completed.\n")
cat("R version detected: ", R.version.string, "\n", sep = "")
cat("Shiny version detected: ", as.character(utils::packageVersion("shiny")), "\n", sep = "")
if (!identical(paste(R.version$major, R.version$minor, sep = "."), "4.6.0")) {
  warning("The release candidate must be frozen under R 4.6.0.", call. = FALSE)
}
if (!identical(as.character(utils::packageVersion("shiny")), "1.14.0")) {
  warning("The release candidate must be frozen under Shiny 1.14.0.", call. = FALSE)
}
