source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "app_data.R"), encoding = "UTF-8")
source(file.path("R", "app_reporting.R"), encoding = "UTF-8")

cat("\n=== RIEGOAZUAY DSS: ENVIRONMENT FREEZE ===\n")
checks <- list()
add_freeze_check <- function(id, description, pass, observed, expected) {
  checks[[length(checks) + 1L]] <<- new_test_row(id, description, isTRUE(pass), observed, expected)
}

version <- package_version_string()
tag <- trimws(readLines("RELEASE_TAG", warn = FALSE, encoding = "UTF-8")[[1]])
allowed_versions <- c("1.0.0-rc6", "1.0.0")
add_freeze_check("FRZ001", "Version is an authorised release-candidate/final value", version %in% allowed_versions, version, paste(allowed_versions, collapse = " or "))
add_freeze_check("FRZ002", "Release tag matches VERSION", identical(tag, paste0("RiegoAzuay-DSS-v", version)), tag, paste0("RiegoAzuay-DSS-v", version))

r_version <- paste(R.version$major, R.version$minor, sep = ".")
add_freeze_check("FRZ003", "R version is exactly 4.6.0", identical(r_version, "4.6.0"), r_version, "4.6.0")
shiny_version <- if (requireNamespace("shiny", quietly = TRUE)) as.character(utils::packageVersion("shiny")) else "missing"
add_freeze_check("FRZ004", "Shiny version is exactly 1.14.0", identical(shiny_version, "1.14.0"), shiny_version, "1.14.0")

runtime_register_path <- file.path("config", "runtime_packages_v1.csv")
runtime_register <- utils::read.csv(runtime_register_path, stringsAsFactors = FALSE, check.names = FALSE)
runtime_packages <- unique(trimws(as.character(runtime_register$package)))
runtime_packages <- runtime_packages[nzchar(runtime_packages)]
missing_packages <- runtime_packages[!vapply(runtime_packages, requireNamespace, logical(1), quietly = TRUE)]
add_freeze_check("FRZ005", "All registered runtime packages are installed", length(missing_packages) == 0L, paste(missing_packages, collapse = " | "), "none missing")

preconditions <- dplyr::bind_rows(checks)
if (!all(preconditions$status == "PASS")) {
  print_test_summary(preconditions, "Freeze refused: release-candidate preconditions failed.")
}

if (!requireNamespace("renv", quietly = TRUE)) {
  utils::install.packages("renv", repos = "https://cloud.r-project.org")
}

# Snapshot the accepted author environment without switching the project to a new empty library.
renv::snapshot(project = ".", packages = runtime_packages, prompt = FALSE, force = TRUE)

dir.create("reproducibility", recursive = TRUE, showWarnings = FALSE)
writeLines(capture.output(sessionInfo()), file.path("reproducibility", "sessionInfo.txt"), useBytes = TRUE)

lock_exists <- file.exists("renv.lock") &&
  is.finite(file.info("renv.lock")$size) &&
  file.info("renv.lock")$size > 0
add_freeze_check(
  "FRZ006",
  "renv.lock was created and is nonempty",
  lock_exists,
  if (file.exists("renv.lock")) file.info("renv.lock")$size else 0,
  ">0 bytes"
)

lock <- if (lock_exists) jsonlite::read_json("renv.lock", simplifyVector = FALSE) else list()
lock_r <- if (!is.null(lock$R$Version)) as.character(lock$R$Version) else ""
lock_packages <- if (!is.null(lock$Packages)) names(lock$Packages) else character(0)
lock_shiny <- if ("shiny" %in% lock_packages) as.character(lock$Packages$shiny$Version) else ""
add_freeze_check("FRZ007", "renv.lock records R 4.6.0", identical(lock_r, "4.6.0"), lock_r, "4.6.0")
add_freeze_check("FRZ008", "renv.lock records Shiny 1.14.0", identical(lock_shiny, "1.14.0"), lock_shiny, "1.14.0")
missing_lock <- setdiff(runtime_packages, lock_packages)
add_freeze_check("FRZ009", "renv.lock contains every registered runtime package", length(missing_lock) == 0L, paste(missing_lock, collapse = " | "), "none missing")

lock_field <- function(entry, field) {
  if (is.null(entry) || is.null(entry[[field]]) || length(entry[[field]]) == 0L) return("")
  value <- as.character(entry[[field]])
  value <- value[!is.na(value)]
  paste(value, collapse = ";")
}

description_field <- function(desc, field) {
  if (is.null(desc) || is.null(desc[[field]]) || length(desc[[field]]) == 0L) return("")
  value <- as.character(desc[[field]])
  value <- value[!is.na(value)]
  paste(value, collapse = ";")
}

build_locked_package_inventory <- function(lock, runtime_packages) {
  package_names <- if (!is.null(lock$Packages)) sort(names(lock$Packages)) else character(0)
  empty_inventory <- data.frame(
    Package = character(0),
    InstalledVersion = character(0),
    LockVersion = character(0),
    VersionMatchesLock = logical(0),
    LibPath = character(0),
    PackagePath = character(0),
    Built = character(0),
    LockSource = character(0),
    LockRepository = character(0),
    LockRemoteType = character(0),
    LockHash = character(0),
    RuntimeRegistered = logical(0),
    stringsAsFactors = FALSE
  )
  if (!length(package_names)) return(empty_inventory)

  rows <- lapply(package_names, function(package_name) {
    package_path <- tryCatch(
      find.package(package_name, quiet = TRUE),
      error = function(e) character(0)
    )
    package_path <- if (length(package_path)) {
      normalizePath(package_path[[1]], winslash = "/", mustWork = FALSE)
    } else {
      ""
    }
    lib_path <- if (nzchar(package_path)) dirname(package_path) else ""
    desc <- if (nzchar(package_path)) {
      tryCatch(
        utils::packageDescription(package_name, lib.loc = lib_path),
        error = function(e) NULL
      )
    } else {
      NULL
    }

    installed_version <- description_field(desc, "Version")
    if (!nzchar(installed_version)) {
      installed_version <- tryCatch(
        as.character(utils::packageVersion(package_name)),
        error = function(e) ""
      )
    }

    entry <- lock$Packages[[package_name]]
    lock_version <- lock_field(entry, "Version")
    data.frame(
      Package = package_name,
      InstalledVersion = installed_version,
      LockVersion = lock_version,
      VersionMatchesLock = nzchar(installed_version) &&
        nzchar(lock_version) &&
        identical(installed_version, lock_version),
      LibPath = lib_path,
      PackagePath = package_path,
      Built = description_field(desc, "Built"),
      LockSource = lock_field(entry, "Source"),
      LockRepository = lock_field(entry, "Repository"),
      LockRemoteType = lock_field(entry, "RemoteType"),
      LockHash = lock_field(entry, "Hash"),
      RuntimeRegistered = package_name %in% runtime_packages,
      stringsAsFactors = FALSE
    )
  })

  inventory <- do.call(rbind, rows)
  rownames(inventory) <- NULL
  inventory
}

package_inventory <- build_locked_package_inventory(lock, runtime_packages)
inventory_path <- file.path("reproducibility", "installed_package_versions.csv")
write_csv_utf8(package_inventory, inventory_path)

required_inventory_columns <- c(
  "Package", "InstalledVersion", "LockVersion", "VersionMatchesLock",
  "LibPath", "PackagePath", "Built", "LockSource", "LockRepository",
  "LockRemoteType", "LockHash", "RuntimeRegistered"
)
inventory_columns_ok <- all(required_inventory_columns %in% names(package_inventory))
inventory_package_set_ok <- length(lock_packages) > 0L &&
  nrow(package_inventory) == length(lock_packages) &&
  !anyDuplicated(package_inventory$Package) &&
  setequal(package_inventory$Package, lock_packages)
inventory_versions_ok <- nrow(package_inventory) > 0L &&
  all(package_inventory$VersionMatchesLock %in% TRUE)
inventory_paths_ok <- nrow(package_inventory) > 0L &&
  all(nzchar(package_inventory$LibPath)) &&
  all(nzchar(package_inventory$PackagePath))
inventory_ok <- inventory_columns_ok &&
  inventory_package_set_ok &&
  inventory_versions_ok &&
  inventory_paths_ok

mismatched_inventory <- if (nrow(package_inventory)) {
  package_inventory$Package[!package_inventory$VersionMatchesLock]
} else {
  character(0)
}
inventory_observed <- paste0(
  "rows=", nrow(package_inventory),
  "; lock_packages=", length(lock_packages),
  "; missing=", paste(setdiff(lock_packages, package_inventory$Package), collapse = "|"),
  "; mismatched=", paste(mismatched_inventory, collapse = "|")
)

freeze_metadata <- data.frame(
  field = c(
    "software", "version", "release_tag", "candidate_status",
    "created_utc", "r_version", "shiny_version", "renv_version",
    "renv_lock_sha256"
  ),
  value = c(
    "RiegoAzuay DSS",
    version,
    tag,
    if (identical(version, "1.0.0")) {
      "final citable release"
    } else {
      "release candidate; not final citable release"
    },
    format(Sys.time(), tz = "UTC", usetz = TRUE),
    R.version.string,
    shiny_version,
    as.character(utils::packageVersion("renv")),
    if (lock_exists) hash_file_sha256("renv.lock") else ""
  ),
  stringsAsFactors = FALSE
)
write_csv_utf8(freeze_metadata, file.path("reproducibility", "ENVIRONMENT_FREEZE_METADATA.csv"))
writeLines(
  c(
    "RiegoAzuay DSS environment freeze: PASS",
    paste0("Version: ", version),
    paste0("Release tag: ", tag),
    paste0("R: ", R.version.string),
    paste0("Shiny: ", shiny_version),
    paste0("renv.lock SHA-256: ", if (lock_exists) hash_file_sha256("renv.lock") else "missing"),
    if (identical(version, "1.0.0")) {
      "Boundary: final public software release; the version-specific DOI is assigned by the external archival record."
    } else {
      "Boundary: a release-candidate freeze is not an immutable public release and has no asserted DOI."
    }
  ),
  file.path("reproducibility", "ENVIRONMENT_FREEZE_STATUS.txt"),
  useBytes = TRUE
)

add_freeze_check(
  "FRZ010",
  "sessionInfo.txt is archived",
  file.exists(file.path("reproducibility", "sessionInfo.txt")) &&
    file.info(file.path("reproducibility", "sessionInfo.txt"))$size > 0,
  if (file.exists(file.path("reproducibility", "sessionInfo.txt"))) {
    file.info(file.path("reproducibility", "sessionInfo.txt"))$size
  } else {
    0
  },
  ">0 bytes"
)
add_freeze_check(
  "FRZ011",
  "Lockfile package inventory is complete, schema-safe, and version-matched",
  file.exists(inventory_path) &&
    file.info(inventory_path)$size > 0 &&
    inventory_ok,
  inventory_observed,
  "one complete version-matched row per lockfile package"
)

write_csv_utf8(dplyr::bind_rows(checks), file.path("reproducibility", "environment_freeze_tests.csv"))
write_sha256_manifest("reproducibility", file.path("reproducibility", "REPRODUCIBILITY_MANIFEST_SHA256.csv"))
manifest_checks <- verify_sha256_manifest(
  "reproducibility",
  file.path("reproducibility", "REPRODUCIBILITY_MANIFEST_SHA256.csv"),
  stop_on_failure = FALSE
)
add_freeze_check(
  "FRZ012",
  "Reproducibility manifest verifies",
  nrow(manifest_checks) > 0L && all(manifest_checks$status == "PASS"),
  paste(manifest_checks$check, manifest_checks$status, collapse = " | "),
  "all PASS"
)

all_checks <- dplyr::bind_rows(checks)
write_csv_utf8(all_checks, file.path("reproducibility", "environment_freeze_tests.csv"))

# Regenerate after writing the final 12-row test table, then verify the exact frozen state.
write_sha256_manifest("reproducibility", file.path("reproducibility", "REPRODUCIBILITY_MANIFEST_SHA256.csv"))
final_manifest_checks <- verify_sha256_manifest(
  "reproducibility",
  file.path("reproducibility", "REPRODUCIBILITY_MANIFEST_SHA256.csv"),
  stop_on_failure = FALSE
)
if (!nrow(final_manifest_checks) || !all(final_manifest_checks$status == "PASS")) {
  stop(
    "Freeze refused: final reproducibility manifest did not verify after the final test table was written.",
    call. = FALSE
  )
}

print_test_summary(all_checks, "RESULTADO GENERAL: LA CONGELACIÓN DEL ENTORNO REGISTRÓ FALLOS.")
cat("RESULTADO GENERAL: ENTORNO DE LA CANDIDATA CONGELADO.\n")
