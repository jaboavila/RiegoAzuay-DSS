source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "app_data.R"), encoding = "UTF-8")
source(file.path("R", "app_reporting.R"), encoding = "UTF-8")

cat("\n=== PREPARE RELEASE-CANDIDATE PACKAGES ===\n")
version <- package_version_string()
if (!version %in% c("1.0.0-rc6", "1.0.0")) {
  stop("Release packaging refused for VERSION: ", version, call. = FALSE)
}

required_freeze <- c(
  "renv.lock",
  file.path("reproducibility", "sessionInfo.txt"),
  file.path("reproducibility", "installed_package_versions.csv"),
  file.path("reproducibility", "ENVIRONMENT_FREEZE_STATUS.txt"),
  file.path("reproducibility", "environment_freeze_tests.csv"),
  file.path("reproducibility", "REPRODUCIBILITY_MANIFEST_SHA256.csv")
)
missing_freeze <- required_freeze[!file.exists(required_freeze)]
if (length(missing_freeze)) {
  stop(
    "Release packaging refused: missing freeze evidence: ",
    paste(missing_freeze, collapse = ", "),
    call. = FALSE
  )
}
verify_sha256_manifest(
  "reproducibility",
  file.path("reproducibility", "REPRODUCIBILITY_MANIFEST_SHA256.csv")
)

copy_release_tree <- function(staging, include_restricted = FALSE) {
  exclude_patterns <- c(
    "^outputs($|/)",
    "^renv/library($|/)",
    "^renv/staging($|/)",
    "^\\.Rproj\\.user($|/)",
    "^\\.git($|/)",
    "^PACKAGE_MANIFEST_SHA256\\.csv$",
    "^RiegoAzuay-DSS-v.*_BUILD_AUDIT\\.txt$",
    "^RC6_HOTFIX($|/)"
  )
  if (!include_restricted) {
    exclude_patterns <- c(
      exclude_patterns,
      "^data/restricted($|/)",
      "^data/audit/territorial_selector_audit_final_v1\\.csv$",
      "^data/audit/territorial_name_ambiguity_audit_v1\\.csv$",
      "^reproducibility/alpha13_tests($|/)",
      "^reproducibility/AUTHOR_RUNTIME_",
      "^reproducibility/AUTHOR_INTERFACE_ACCEPTANCE_",
      "^reproducibility/STATIC_ASSEMBLY_AUDIT",
      "^docs/PATCH_ALPHA",
      "^docs/PATCH_RC2_",
      "^docs/PATCH_RC3_",
      "^docs/AUTHOR_INTERFACE_AUDIT\\.md$",
      "^docs/ALPHA_GAP_REGISTER\\.md$"
    )
  }

  all_files <- list.files(
    ".",
    recursive = TRUE,
    full.names = FALSE,
    all.files = TRUE,
    no.. = TRUE
  )
  all_files <- all_files[file.info(all_files)$isdir %in% FALSE]
  normal <- gsub("\\\\", "/", all_files)
  keep <- rep(TRUE, length(normal))
  for (pattern in exclude_patterns) {
    keep <- keep & !grepl(pattern, normal)
  }

  for (f in all_files[keep]) {
    target <- file.path(staging, f)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(file.copy(f, target, overwrite = TRUE))) {
      stop("Could not copy release file: ", f, call. = FALSE)
    }
  }
}

write_public_restricted_notice <- function(staging) {
  restricted_dir <- file.path(staging, "data", "restricted")
  dir.create(restricted_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "# Restricted institutional registry intentionally absent",
      "",
      "The Provincial Government of Azuay selector is not distributed in the public package.",
      "The public application starts with a synthetic five-record demonstration registry.",
      "Aggregate selector counts are retained in data/audit/territorial_selector_summary.json",
      "and data/audit/territorial_selector_public_summary_v1.csv.",
      "The detailed 159-system selector and record-level territorial audit are available only",
      "in the controlled reviewer package and must not be redistributed."
    ),
    file.path(restricted_dir, "README.md"),
    useBytes = TRUE
  )
}

sanitize_distributed_inventory <- function(staging) {
  path <- file.path(staging, "reproducibility", "installed_package_versions.csv")
  if (!file.exists(path)) {
    stop("Distributed package is missing installed_package_versions.csv.", call. = FALSE)
  }
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  required <- c(
    "Package", "InstalledVersion", "LockVersion", "VersionMatchesLock",
    "Built", "LockSource", "LockRepository", "LockRemoteType",
    "LockHash", "RuntimeRegistered"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Package inventory cannot be sanitised; missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  x <- x[, required, drop = FALSE]
  write_csv_utf8(x, path)
  invisible(path)
}

refresh_distributed_reproducibility_manifest <- function(staging) {
  root <- file.path(staging, "reproducibility")
  path <- file.path(root, "REPRODUCIBILITY_MANIFEST_SHA256.csv")
  if (!dir.exists(root)) {
    stop("Distributed package is missing reproducibility/.", call. = FALSE)
  }
  write_sha256_manifest(root, path)
  checks <- verify_sha256_manifest(root, path, stop_on_failure = FALSE)
  if (!nrow(checks) || any(checks$status != "PASS")) {
    stop("Distributed reproducibility manifest did not verify.", call. = FALSE)
  }
  invisible(checks)
}

assert_no_local_workstation_paths <- function(staging) {
  text_extensions <- c(
    "R", "Rmd", "md", "txt", "csv", "json", "yml", "yaml", "cff", "lock", "Rproj"
  )
  files <- list.files(staging, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  ext <- tools::file_ext(files)
  candidates <- files[ext %in% text_extensions | basename(files) %in% c("VERSION", "RELEASE_TAG", ".gitignore")]
  offending <- character(0)
  forbidden_fragments <- c(
    paste0("C:", "/Users/"),
    paste0("C:", "\\", "Users", "\\"),
    paste0("C:", "/ARCHIVOS"),
    paste0("aaDoctorado", " Valencia")
  )
  for (f in candidates) {
    lines <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
    txt <- paste(lines, collapse = "\n")
    if (any(vapply(forbidden_fragments, function(z) grepl(z, txt, fixed = TRUE), logical(1)))) {
      offending <- c(offending, substring(normalizePath(f, winslash = "/", mustWork = FALSE), nchar(normalizePath(staging, winslash = "/", mustWork = TRUE)) + 2L))
    }
  }
  if (length(offending)) {
    stop(
      "Distributed package contains author-workstation paths: ",
      paste(unique(offending), collapse = " | "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

prepare_distributed_tree <- function(staging, include_restricted) {
  if (!include_restricted) {
    write_public_restricted_notice(staging)
    required_public <- c(
      file.path(staging, "data", "audit", "territorial_selector_summary.json"),
      file.path(staging, "data", "audit", "territorial_selector_public_summary_v1.csv"),
      file.path(staging, "data", "restricted", "README.md")
    )
    missing_public <- required_public[!file.exists(required_public)]
    if (length(missing_public)) {
      stop(
        "Public package is missing safe territorial summary assets: ",
        paste(missing_public, collapse = ", "),
        call. = FALSE
      )
    }
  }

  sanitize_distributed_inventory(staging)
  refresh_distributed_reproducibility_manifest(staging)
  assert_no_local_workstation_paths(staging)
}

build_package <- function(suffix, include_restricted, notice_lines) {
  release_name <- paste0("RiegoAzuay-DSS-v", version, "-", suffix)
  staging_parent <- tempfile(paste0("riegoazuay_", suffix, "_"))
  staging <- file.path(staging_parent, release_name)
  dir.create(staging, recursive = TRUE, showWarnings = FALSE)

  copy_release_tree(staging, include_restricted = include_restricted)
  prepare_distributed_tree(staging, include_restricted = include_restricted)

  notice_name <- if (include_restricted) {
    "REVIEWER_CONTROLLED_NOTICE.txt"
  } else {
    "PUBLIC_PREVIEW_NOTICE.txt"
  }
  writeLines(notice_lines, file.path(staging, notice_name), useBytes = TRUE)

  write_sha256_manifest(staging)
  verify_sha256_manifest(staging)

  out_dir <- file.path("outputs", "release_candidates")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_zip <- file.path(out_dir, paste0(release_name, ".zip"))
  out_zip_abs <- write_verified_zip_app(staging, out_zip)
  out_sha <- hash_file_sha256(out_zip_abs)
  writeLines(
    c(
      paste0(out_sha, "  ", basename(out_zip_abs)),
      paste0("Version: ", version)
    ),
    paste0(out_zip_abs, "_SHA256.txt"),
    useBytes = TRUE
  )
  unlink(staging_parent, recursive = TRUE, force = TRUE)

  data.frame(
    package = suffix,
    path = out_zip_abs,
    sha256 = out_sha,
    restricted_registry_included = include_restricted,
    stringsAsFactors = FALSE
  )
}

public_notice <- c(
  "RiegoAzuay DSS — non-confidential public-preview package",
  paste0("Software version: ", version),
  "This is a release candidate, not the citable v1.0.0 release.",
  "The restricted Provincial Government of Azuay registry and record-level territorial audits are intentionally absent.",
  "The application starts with a synthetic demonstration registry and retains only aggregate selector counts.",
  "Author-workstation package paths have been removed from the distributed inventory.",
  "Public publication is not authorised until the authors approve a software licence and create an immutable repository release.",
  "No DOI is asserted because none has yet been minted."
)

review_notice <- c(
  "RiegoAzuay DSS — controlled reviewer package",
  paste0("Software version: ", version),
  "This package contains the processed 159-system restricted selector and detailed territorial audit for confidential peer review and author testing.",
  "It must not be uploaded to a public repository or redistributed.",
  "The source institutional workbook is not included.",
  "Author-workstation package paths have been removed from the distributed inventory.",
  "This is a release candidate, not the citable v1.0.0 release, and no DOI is asserted."
)

index <- dplyr::bind_rows(
  build_package("public-preview-NOT-FOR-PUBLICATION", FALSE, public_notice),
  build_package("reviewer-controlled", TRUE, review_notice)
)
write_csv_utf8(
  index,
  file.path("outputs", "release_candidates", "RELEASE_CANDIDATE_PACKAGE_INDEX.csv")
)
cat("Release-candidate packages prepared under outputs/release_candidates/.\n")
