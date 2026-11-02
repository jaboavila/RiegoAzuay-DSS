source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "app_data.R"), encoding = "UTF-8")
source(file.path("R", "app_reporting.R"), encoding = "UTF-8")

cat("\n=== RIEGOAZUAY DSS: PACKAGING AND PATH-RESOLVED MANIFEST GATE ===\n")

tests <- list()
add_gate <- function(id, description, pass, observed, expected) {
  tests[[length(tests) + 1L]] <<- new_test_row(id, description, isTRUE(pass), observed, expected)
}

out_dir <- file.path("outputs", "packaging_gate")
if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE, force = TRUE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tmp_parent <- tempfile("riegoazuay_manifest_gate_")
staging <- file.path(tmp_parent, "staging")
dir.create(file.path(staging, "nested"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(staging, "another"), recursive = TRUE, showWarnings = FALSE)
writeLines("root", file.path(staging, "root.txt"), useBytes = TRUE)
writeLines("duplicate-content", file.path(staging, "nested", "a.txt"), useBytes = TRUE)
writeLines("duplicate-content", file.path(staging, "nested", "b.txt"), useBytes = TRUE)
file.create(file.path(staging, "another", "empty.txt"))

manifest <- write_sha256_manifest(staging)
manifest_checks <- verify_sha256_manifest(staging, stop_on_failure = FALSE)
expected_paths <- sort(c("another/empty.txt", "nested/a.txt", "nested/b.txt", "root.txt"))
add_gate("MAN001", "Manifest contains one row per staged file", nrow(manifest) == length(expected_paths), nrow(manifest), length(expected_paths))
add_gate("MAN002", "Manifest paths are nonblank", all(nzchar(trimws(manifest$relative_path))), paste(manifest$relative_path, collapse = " | "), "all nonblank")
add_gate("MAN003", "Manifest paths are unique", !anyDuplicated(manifest$relative_path), length(unique(manifest$relative_path)), length(manifest$relative_path))
add_gate("MAN004", "Manifest paths are relative and safe", all(!grepl("^([A-Za-z]:|/|\\\\)", manifest$relative_path)) && all(!vapply(strsplit(manifest$relative_path, "/", fixed = TRUE), function(x) ".." %in% x, logical(1))), paste(manifest$relative_path, collapse = " | "), "relative paths without parent traversal")
add_gate("MAN005", "Manifest has exact staged-file coverage", identical(sort(manifest$relative_path), expected_paths), paste(sort(manifest$relative_path), collapse = " | "), paste(expected_paths, collapse = " | "))
add_gate("MAN006", "Manifest verification passes all path, size, and hash checks", all(manifest_checks$status == "PASS"), paste(manifest_checks$check, manifest_checks$status, collapse = " | "), "all PASS")
add_gate("MAN007", "Duplicate-content files remain distinct path entries", all(c("nested/a.txt", "nested/b.txt") %in% manifest$relative_path), paste(manifest$relative_path[manifest$sha256 == manifest$sha256[manifest$relative_path == "nested/a.txt"]], collapse = " | "), "nested/a.txt and nested/b.txt")

zip_path <- file.path(out_dir, "manifest_gate_synthetic_bundle.zip")
zip_abs <- write_verified_zip_app(staging, zip_path)
extract_dir <- file.path(tmp_parent, "extracted")
dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
utils::unzip(zip_abs, exdir = extract_dir)
zip_manifest_checks <- verify_sha256_manifest(extract_dir, stop_on_failure = FALSE)
add_gate("MAN008", "Verified ZIP contains every staged path", setequal(utils::unzip(zip_abs, list = TRUE)$Name, c(expected_paths, "MANIFEST_SHA256.csv")), paste(utils::unzip(zip_abs, list = TRUE)$Name, collapse = " | "), "staged files plus manifest")
add_gate("MAN009", "Manifest remains valid after ZIP extraction", all(zip_manifest_checks$status == "PASS"), paste(zip_manifest_checks$check, zip_manifest_checks$status, collapse = " | "), "all PASS")

live_zip <- file.path("reproducibility", "live_source_audit_20261029T221201Z_MANIFEST_FIXED.zip")
live_sha_file <- file.path("reproducibility", "live_source_audit_20261029T221201Z_MANIFEST_FIXED_SHA256.txt")
add_gate("LIVE001", "Repaired live-source audit bundle is present", file.exists(live_zip) && file.info(live_zip)$size > 0, if (file.exists(live_zip)) file.info(live_zip)$size else 0, ">0 bytes")
expected_live_sha <- if (file.exists(live_sha_file)) strsplit(readLines(live_sha_file, warn = FALSE, encoding = "UTF-8")[[1]], "[[:space:]]+")[[1]][[1]] else ""
actual_live_sha <- if (file.exists(live_zip)) hash_file_sha256(live_zip) else ""
add_gate("LIVE002", "Repaired live-source audit bundle matches its external SHA-256", nzchar(expected_live_sha) && identical(tolower(actual_live_sha), tolower(expected_live_sha)), paste(actual_live_sha, expected_live_sha, sep = " / "), "matching SHA-256")

live_extract <- file.path(tmp_parent, "live_extract")
dir.create(live_extract, recursive = TRUE, showWarnings = FALSE)
if (file.exists(live_zip)) utils::unzip(live_zip, exdir = live_extract)
live_status_path <- file.path(live_extract, "AUDIT_STATUS.txt")
live_tests_path <- file.path(live_extract, "live_source_audit_tests.csv")
live_status_text <- if (file.exists(live_status_path)) paste(readLines(live_status_path, warn = FALSE, encoding = "UTF-8"), collapse = " | ") else ""
live_tests <- if (file.exists(live_tests_path)) as.data.frame(read_csv_utf8(live_tests_path), stringsAsFactors = FALSE) else data.frame()
add_gate("LIVE003", "Archived live-source audit status remains PASS", grepl("audit: PASS", live_status_text, fixed = TRUE), live_status_text, "audit: PASS")
add_gate("LIVE004", "Archived live-source audit retains 36/36 passing checks", nrow(live_tests) == 36L && all(live_tests$status == "PASS"), paste(sum(live_tests$status == "PASS"), "/", nrow(live_tests)), "36 / 36")
live_manifest_checks <- if (dir.exists(live_extract)) verify_sha256_manifest(live_extract, stop_on_failure = FALSE) else data.frame(status = "FAIL")
add_gate("LIVE005", "Repaired live-source audit manifest verifies after extraction", nrow(live_manifest_checks) > 0L && all(live_manifest_checks$status == "PASS"), if (nrow(live_manifest_checks)) paste(live_manifest_checks$check, live_manifest_checks$status, collapse = " | ") else "no checks", "all PASS")
add_gate("LIVE006", "Original defective manifest and repair note are preserved for traceability", file.exists(file.path(live_extract, "MANIFEST_SHA256_ORIGINAL_BLANK_PATHS.csv")) && file.exists(file.path(live_extract, "MANIFEST_REPAIR_NOTE.txt")), paste(file.exists(file.path(live_extract, "MANIFEST_SHA256_ORIGINAL_BLANK_PATHS.csv")), file.exists(file.path(live_extract, "MANIFEST_REPAIR_NOTE.txt")), sep = " / "), "TRUE / TRUE")

all_tests <- dplyr::bind_rows(tests)
write_csv_utf8(all_tests, file.path(out_dir, "packaging_manifest_gate_tests.csv"))
status <- if (all(all_tests$status == "PASS")) "PASS" else "FAIL"
writeLines(c(
  paste0("RiegoAzuay DSS packaging and manifest gate: ", status),
  paste0("Checks passed: ", sum(all_tests$status == "PASS"), " / ", nrow(all_tests)),
  paste0("Software version: ", package_version_string()),
  "Boundary: this gate validates packaging, path-resolved manifests, hashes, and preservation of the approved live-source evidence."
), file.path(out_dir, "PACKAGING_GATE_STATUS.txt"), useBytes = TRUE)

out_bundle <- file.path(out_dir, "RiegoAzuay_packaging_gate_evidence.zip")
# Package a copy outside the output directory to avoid self-inclusion.
evidence_staging <- tempfile("riegoazuay_packaging_evidence_")
dir.create(evidence_staging, recursive = TRUE, showWarnings = FALSE)
for (f in list.files(out_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)) {
  if (basename(f) != basename(out_bundle) && basename(f) != "MANIFEST_SHA256.csv") {
    file.copy(f, file.path(evidence_staging, basename(f)), overwrite = TRUE)
  }
}
write_sha256_manifest(evidence_staging)
verify_sha256_manifest(evidence_staging)
out_bundle_abs <- write_verified_zip_app(evidence_staging, out_bundle)
out_bundle_sha <- hash_file_sha256(out_bundle_abs)
writeLines(
  c(
    paste0(out_bundle_sha, "  ", basename(out_bundle_abs)),
    paste0("Packaging gate status: ", status),
    paste0("Checks passed: ", sum(all_tests$status == "PASS"), " / ", nrow(all_tests))
  ),
  file.path(out_dir, "RiegoAzuay_packaging_gate_evidence_SHA256.txt"),
  useBytes = TRUE
)
unlink(c(tmp_parent, evidence_staging), recursive = TRUE, force = TRUE)

print_test_summary(all_tests, "RESULTADO GENERAL: LA COMPUERTA DE EMPAQUETADO REGISTRÓ FALLOS.")
cat("RESULTADO GENERAL: EMPAQUETADO Y MANIFIESTOS SUPERADOS.\n")
cat("Evidence bundle: ", normalizePath(out_bundle_abs, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat("Evidence SHA-256: ", out_bundle_sha, "\n", sep = "")
