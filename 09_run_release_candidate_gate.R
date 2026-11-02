source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "app_data.R"), encoding = "UTF-8")
source(file.path("R", "app_reporting.R"), encoding = "UTF-8")

cat("\n=== RIEGOAZUAY DSS v1.0.0-rc6: RELEASE-CANDIDATE GATE ===\n")
checks <- list()
add_rc <- function(id, description, pass, observed, expected) {
  checks[[length(checks) + 1L]] <<- new_test_row(
    id, description, isTRUE(pass), observed, expected
  )
}

version <- package_version_string()
tag <- trimws(readLines("RELEASE_TAG", warn = FALSE, encoding = "UTF-8")[[1]])
add_rc("RC001", "VERSION is 1.0.0-rc6", identical(version, "1.0.0-rc6"), version, "1.0.0-rc6")
add_rc(
  "RC002",
  "Release tag matches the release candidate",
  identical(tag, "RiegoAzuay-DSS-v1.0.0-rc6"),
  tag,
  "RiegoAzuay-DSS-v1.0.0-rc6"
)

accept_path <- file.path(
  "reproducibility",
  "AUTHOR_INTERFACE_ACCEPTANCE_ALPHA11_20261029.txt"
)
accept_text <- if (file.exists(accept_path)) {
  paste(readLines(accept_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
} else {
  ""
}
add_rc(
  "RC003",
  "Author interface acceptance is archived",
  file.exists(accept_path) &&
    grepl("Todos los ítems anteriores están en PASS", accept_text, fixed = TRUE) &&
    grepl("= SÍ", accept_text, fixed = TRUE),
  if (file.exists(accept_path)) "archived PASS" else "missing",
  "archived PASS"
)

read_test_file <- function(path) {
  if (file.exists(path)) {
    as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  } else {
    data.frame()
  }
}

current_static_path <- file.path("outputs", "tests", "static_preflight_tests.csv")
current_scientific_path <- file.path("outputs", "tests", "offline_scientific_tests.csv")
current_integration_path <- file.path("outputs", "tests", "integration_smoke_tests.csv")
st <- read_test_file(current_static_path)
sc <- read_test_file(current_scientific_path)
it <- read_test_file(current_integration_path)
add_rc(
  "RC004",
  "Current release-candidate preflight is 208/208 PASS",
  nrow(st) == 208L && all(st$status == "PASS"),
  paste(sum(st$status == "PASS"), "/", nrow(st)),
  "208 / 208"
)
add_rc(
  "RC005",
  "Current release-candidate scientific regression is 108/108 PASS",
  nrow(sc) == 108L && all(sc$status == "PASS"),
  paste(sum(sc$status == "PASS"), "/", nrow(sc)),
  "108 / 108"
)
add_rc(
  "RC006",
  "Current release-candidate integration suite is 84/84 PASS",
  nrow(it) == 84L && all(it$status == "PASS"),
  paste(sum(it$status == "PASS"), "/", nrow(it)),
  "84 / 84"
)

live_zip <- file.path(
  "reproducibility",
  "live_source_audit_20261029T221201Z_MANIFEST_FIXED.zip"
)
live_sha_file <- file.path(
  "reproducibility",
  "live_source_audit_20261029T221201Z_MANIFEST_FIXED_SHA256.txt"
)
expected_live_sha <- if (file.exists(live_sha_file)) {
  strsplit(
    readLines(live_sha_file, warn = FALSE, encoding = "UTF-8")[[1]],
    "[[:space:]]+"
  )[[1]][[1]]
} else {
  ""
}
actual_live_sha <- if (file.exists(live_zip)) hash_file_sha256(live_zip) else ""
add_rc(
  "RC007",
  "Approved live-source bundle hash matches",
  nzchar(expected_live_sha) &&
    identical(tolower(expected_live_sha), tolower(actual_live_sha)),
  paste(actual_live_sha, expected_live_sha, sep = " / "),
  "matching"
)
live_tmp <- tempfile("rc_live_")
dir.create(live_tmp, recursive = TRUE)
if (file.exists(live_zip)) utils::unzip(live_zip, exdir = live_tmp)
live_tests <- read_test_file(file.path(live_tmp, "live_source_audit_tests.csv"))
add_rc(
  "RC008",
  "Approved live-source gate retains 36/36 PASS",
  nrow(live_tests) == 36L && all(live_tests$status == "PASS"),
  paste(sum(live_tests$status == "PASS"), "/", nrow(live_tests)),
  "36 / 36"
)
live_manifest <- if (dir.exists(live_tmp)) {
  verify_sha256_manifest(live_tmp, stop_on_failure = FALSE)
} else {
  data.frame(status = "FAIL")
}
add_rc(
  "RC009",
  "Approved live-source bundle manifest verifies",
  nrow(live_manifest) > 0L && all(live_manifest$status == "PASS"),
  if (nrow(live_manifest)) {
    paste(live_manifest$check, live_manifest$status, collapse = " | ")
  } else {
    "no checks"
  },
  "all PASS"
)
unlink(live_tmp, recursive = TRUE, force = TRUE)

pg_zip <- file.path(
  "reproducibility",
  "packaging_gate_approved",
  "RiegoAzuay_packaging_gate_evidence.zip"
)
pg_sha_file <- file.path(
  "reproducibility",
  "packaging_gate_approved",
  "RiegoAzuay_packaging_gate_evidence_SHA256.txt"
)
expected_pg_sha <- if (file.exists(pg_sha_file)) {
  strsplit(
    readLines(pg_sha_file, warn = FALSE, encoding = "UTF-8")[[1]],
    "[[:space:]]+"
  )[[1]][[1]]
} else {
  ""
}
actual_pg_sha <- if (file.exists(pg_zip)) hash_file_sha256(pg_zip) else ""
add_rc(
  "RC010",
  "Author-run packaging gate hash matches",
  nzchar(expected_pg_sha) &&
    identical(tolower(expected_pg_sha), tolower(actual_pg_sha)),
  paste(actual_pg_sha, expected_pg_sha, sep = " / "),
  "matching"
)
pg_tmp <- tempfile("rc_pg_")
dir.create(pg_tmp, recursive = TRUE)
if (file.exists(pg_zip)) utils::unzip(pg_zip, exdir = pg_tmp)
pg_tests <- read_test_file(file.path(pg_tmp, "packaging_manifest_gate_tests.csv"))
add_rc(
  "RC011",
  "Author-run packaging gate retains 15/15 PASS",
  nrow(pg_tests) == 15L && all(pg_tests$status == "PASS"),
  paste(sum(pg_tests$status == "PASS"), "/", nrow(pg_tests)),
  "15 / 15"
)
unlink(pg_tmp, recursive = TRUE, force = TRUE)

freeze_tests <- read_test_file(
  file.path("reproducibility", "environment_freeze_tests.csv")
)
add_rc(
  "RC012",
  "Environment freeze retains 12/12 PASS",
  nrow(freeze_tests) == 12L && all(freeze_tests$status == "PASS"),
  paste(sum(freeze_tests$status == "PASS"), "/", nrow(freeze_tests)),
  "12 / 12"
)
add_rc(
  "RC013",
  "renv.lock exists and is nonempty",
  file.exists("renv.lock") && file.info("renv.lock")$size > 0,
  if (file.exists("renv.lock")) file.info("renv.lock")$size else 0,
  ">0 bytes"
)
lock <- if (file.exists("renv.lock")) {
  jsonlite::read_json("renv.lock", simplifyVector = FALSE)
} else {
  list()
}
add_rc(
  "RC014",
  "Lockfile records R 4.6.0",
  identical(as.character(lock$R$Version), "4.6.0"),
  as.character(lock$R$Version),
  "4.6.0"
)
add_rc(
  "RC015",
  "Lockfile records Shiny 1.14.0",
  !is.null(lock$Packages$shiny) &&
    identical(as.character(lock$Packages$shiny$Version), "1.14.0"),
  if (!is.null(lock$Packages$shiny)) {
    as.character(lock$Packages$shiny$Version)
  } else {
    "missing"
  },
  "1.14.0"
)

index_path <- file.path(
  "outputs",
  "release_candidates",
  "RELEASE_CANDIDATE_PACKAGE_INDEX.csv"
)
idx <- read_test_file(index_path)
public_path <- if (nrow(idx)) {
  idx$path[idx$package == "public-preview-NOT-FOR-PUBLICATION"]
} else {
  character(0)
}
review_path <- if (nrow(idx)) {
  idx$path[idx$package == "reviewer-controlled"]
} else {
  character(0)
}
add_rc(
  "RC016",
  "Public-preview package exists",
  length(public_path) == 1L && file.exists(public_path),
  paste(public_path, collapse = " | "),
  "one existing ZIP"
)
add_rc(
  "RC017",
  "Controlled reviewer package exists",
  length(review_path) == 1L && file.exists(review_path),
  paste(review_path, collapse = " | "),
  "one existing ZIP"
)

extract_package_root <- function(path, prefix) {
  td <- tempfile(prefix)
  dir.create(td, recursive = TRUE)
  utils::unzip(path, exdir = td)
  roots <- list.dirs(td, recursive = FALSE, full.names = TRUE)
  root <- if (length(roots) == 1L) roots[[1]] else td
  list(temp = td, root = root)
}

pub_extract <- if (length(public_path) == 1L && file.exists(public_path)) {
  extract_package_root(public_path, "rc_pub_")
} else {
  list(temp = "", root = "")
}
rev_extract <- if (length(review_path) == 1L && file.exists(review_path)) {
  extract_package_root(review_path, "rc_rev_")
} else {
  list(temp = "", root = "")
}

verify_package_root <- function(root, expect_restricted) {
  if (!nzchar(root) || !dir.exists(root)) {
    return(list(pass = FALSE, manifest = data.frame(), restricted = NA))
  }
  man <- verify_sha256_manifest(root, stop_on_failure = FALSE)
  restricted <- file.exists(
    file.path(root, "data", "restricted", "systems_registry_159_restricted.csv")
  )
  list(
    pass = nrow(man) > 0L &&
      all(man$status == "PASS") &&
      identical(restricted, expect_restricted),
    manifest = man,
    restricted = restricted
  )
}

pub_check <- verify_package_root(pub_extract$root, FALSE)
rev_check <- verify_package_root(rev_extract$root, TRUE)
add_rc(
  "RC018",
  "Public preview verifies and excludes the processed restricted registry",
  isTRUE(pub_check$pass),
  paste("restricted=", pub_check$restricted),
  "manifest PASS; restricted FALSE"
)
add_rc(
  "RC019",
  "Reviewer package verifies and includes the controlled 159-system registry",
  isTRUE(rev_check$pass),
  paste("restricted=", rev_check$restricted),
  "manifest PASS; restricted TRUE"
)

citation <- paste(
  readLines("CITATION.cff", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)
add_rc(
  "RC020",
  "No DOI is asserted before minting",
  !grepl("doi:", citation, ignore.case = TRUE),
  "CITATION.cff inspected",
  "no DOI field"
)
add_rc(
  "RC021",
  "Licence remains explicitly pending before public release",
  file.exists("LICENSE_PENDING.md") &&
    grepl(
      "pending",
      paste(readLines("LICENSE_PENDING.md", warn = FALSE, encoding = "UTF-8"), collapse = " "),
      ignore.case = TRUE
    ),
  "LICENSE_PENDING.md",
  "pending"
)
levels <- as.data.frame(
  read_csv_utf8(file.path("config", "evidence_levels_v1.csv")),
  stringsAsFactors = FALSE
)
add_rc(
  "RC022",
  "E1 does not claim an unimplemented seasonal calculator",
  !grepl(
    "seasonal|estacional",
    paste(
      levels$label_en[levels$level == "E1"],
      levels$authorized_output_en[levels$level == "E1"]
    ),
    ignore.case = TRUE
  ),
  paste(
    levels$label_en[levels$level == "E1"],
    levels$authorized_output_en[levels$level == "E1"],
    sep = " | "
  ),
  "no seasonal calculator claim"
)
add_rc(
  "RC023",
  "E4 remains outside the present prototype",
  grepl(
    "not claimed",
    levels$authorized_output_en[levels$level == "E4"],
    fixed = TRUE
  ),
  levels$authorized_output_en[levels$level == "E4"],
  "contains not claimed"
)
add_rc(
  "RC024",
  "RC4 Methods/data-governance consistency note remains archived",
  file.exists(file.path("docs", "RC4_METHODS_DATA_GOVERNANCE_NOTE.md")),
  file.exists(file.path("docs", "RC4_METHODS_DATA_GOVERNANCE_NOTE.md")),
  TRUE
)

forbidden_public_files <- c(
  file.path("data", "restricted", "systems_registry_159_restricted.csv"),
  file.path("data", "audit", "territorial_selector_audit_final_v1.csv"),
  file.path("data", "audit", "territorial_name_ambiguity_audit_v1.csv")
)
forbidden_present <- if (nzchar(pub_extract$root)) {
  forbidden_public_files[file.exists(file.path(pub_extract$root, forbidden_public_files))]
} else {
  forbidden_public_files
}
add_rc(
  "RC025",
  "Public package excludes the restricted registry and record-level territorial audit files",
  length(forbidden_present) == 0L,
  paste(forbidden_present, collapse = " | "),
  "none"
)

safe_public_files <- c(
  file.path("data", "audit", "territorial_selector_summary.json"),
  file.path("data", "audit", "territorial_selector_public_summary_v1.csv"),
  file.path("data", "restricted", "README.md")
)
safe_public_present <- nzchar(pub_extract$root) &&
  all(file.exists(file.path(pub_extract$root, safe_public_files)))
add_rc(
  "RC026",
  "Public package retains only safe aggregate territorial summaries and an absence notice",
  safe_public_present,
  if (nzchar(pub_extract$root)) {
    paste(file.exists(file.path(pub_extract$root, safe_public_files)), collapse = " | ")
  } else {
    "no public root"
  },
  "TRUE | TRUE | TRUE"
)

text_file_candidates <- function(root) {
  if (!nzchar(root) || !dir.exists(root)) return(character(0))
  files <- list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  ext <- tools::file_ext(files)
  files[
    ext %in% c("R", "Rmd", "md", "txt", "csv", "json", "yml", "yaml", "cff", "lock", "Rproj") |
      basename(files) %in% c("VERSION", "RELEASE_TAG", ".gitignore")
  ]
}

scan_restricted_coordinate_fingerprints <- function(root, registry_path) {
  if (!nzchar(root) || !dir.exists(root) || !file.exists(registry_path)) {
    return(data.frame(file = "missing input", source_row = NA_integer_, stringsAsFactors = FALSE))
  }
  reg <- as.data.frame(read_csv_utf8(registry_path), stringsAsFactors = FALSE)
  reg$lat_key <- sprintf("%.6f", suppressWarnings(as.numeric(reg$latitude)))
  reg$lon_key <- sprintf("%.6f", suppressWarnings(as.numeric(reg$longitude)))
  files <- text_file_candidates(root)
  hits <- list()
  k <- 0L
  root_abs <- normalizePath(root, winslash = "/", mustWork = TRUE)
  for (f in files) {
    txt <- tryCatch(
      paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      error = function(e) ""
    )
    if (!nzchar(txt)) next
    for (i in seq_len(nrow(reg))) {
      if (grepl(reg$lat_key[[i]], txt, fixed = TRUE) &&
          grepl(reg$lon_key[[i]], txt, fixed = TRUE)) {
        k <- k + 1L
        hits[[k]] <- data.frame(
          file = substring(
            normalizePath(f, winslash = "/", mustWork = FALSE),
            nchar(root_abs) + 2L
          ),
          source_row = suppressWarnings(as.integer(reg$source_row[[i]])),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(hits)) {
    return(data.frame(file = character(0), source_row = integer(0), stringsAsFactors = FALSE))
  }
  unique(do.call(rbind, hits))
}

restricted_registry_author <- file.path(
  "data",
  "restricted",
  "systems_registry_159_restricted.csv"
)
coordinate_hits <- scan_restricted_coordinate_fingerprints(
  pub_extract$root,
  restricted_registry_author
)
add_rc(
  "RC027",
  "Public package contains no exact six-decimal institutional coordinate fingerprints",
  nrow(coordinate_hits) == 0L,
  if (nrow(coordinate_hits)) {
    paste(coordinate_hits$file, coordinate_hits$source_row, sep = "#", collapse = " | ")
  } else {
    "none"
  },
  "none"
)

scan_restricted_record_label_fingerprints <- function(root, registry_path, allowed_public_ids = character(0)) {
  empty <- data.frame(file = character(0), public_id = character(0), field = character(0), value = character(0), stringsAsFactors = FALSE)
  if (!nzchar(root) || !dir.exists(root) || !file.exists(registry_path)) {
    return(data.frame(file = "missing input", public_id = "", field = "", value = "", stringsAsFactors = FALSE))
  }
  reg <- as.data.frame(read_csv_utf8(registry_path), stringsAsFactors = FALSE)
  reg <- reg[!reg$public_id %in% allowed_public_ids, , drop = FALSE]
  fields <- intersect(c("system_name", "organization_name"), names(reg))
  files <- text_file_candidates(root)
  root_abs <- normalizePath(root, winslash = "/", mustWork = TRUE)
  hits <- list()
  k <- 0L
  for (f in files) {
    txt <- tryCatch(
      paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      error = function(e) ""
    )
    if (!nzchar(txt)) next
    txt_lower <- tolower(txt)
    for (i in seq_len(nrow(reg))) {
      id_value <- trimws(as.character(reg$public_id[[i]]))
      if (nzchar(id_value) && grepl(id_value, txt, fixed = TRUE)) {
        k <- k + 1L
        hits[[k]] <- data.frame(
          file = substring(normalizePath(f, winslash = "/", mustWork = FALSE), nchar(root_abs) + 2L),
          public_id = id_value,
          field = "public_id",
          value = id_value,
          stringsAsFactors = FALSE
        )
      }
      for (field_name in fields) {
        value <- trimws(as.character(reg[[field_name]][[i]]))
        if (is.na(value) || !nzchar(value) || nchar(value) < 8L) next
        if (grepl(tolower(value), txt_lower, fixed = TRUE)) {
          k <- k + 1L
          hits[[k]] <- data.frame(
            file = substring(normalizePath(f, winslash = "/", mustWork = FALSE), nchar(root_abs) + 2L),
            public_id = id_value,
            field = field_name,
            value = value,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (!length(hits)) return(empty)
  unique(do.call(rbind, hits))
}

allowed_public_record_ids <- unique(c(
  trimws(as.character(load_app_config()$scientific_demo_system_id)),
  "AZU-SR-001", "AZU-SR-159"
))
record_label_hits <- scan_restricted_record_label_fingerprints(
  pub_extract$root,
  restricted_registry_author,
  allowed_public_ids = allowed_public_record_ids
)
add_rc(
  "RC032",
  "Public package contains no non-approved restricted system IDs or record-level names",
  nrow(record_label_hits) == 0L,
  if (nrow(record_label_hits)) {
    paste(record_label_hits$file, record_label_hits$public_id, record_label_hits$field, record_label_hits$value, sep = "#", collapse = " | ")
  } else {
    "none"
  },
  "none"
)

distributed_inventory_ok <- function(root) {
  path <- file.path(root, "reproducibility", "installed_package_versions.csv")
  if (!file.exists(path)) {
    return(list(pass = FALSE, detail = "missing inventory"))
  }
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  required <- c(
    "Package", "InstalledVersion", "LockVersion", "VersionMatchesLock",
    "Built", "LockSource", "LockRepository", "LockRemoteType",
    "LockHash", "RuntimeRegistered"
  )
  text <- paste(unlist(x, use.names = FALSE), collapse = "\n")
  forbidden_fragments <- c(
    paste0("C:", "/Users/"),
    paste0("C:", "\\", "Users", "\\"),
    paste0("C:", "/ARCHIVOS"),
    paste0("aaDoctorado", " Valencia")
  )
  pass <- all(required %in% names(x)) &&
    !any(c("LibPath", "PackagePath") %in% names(x)) &&
    !any(vapply(forbidden_fragments, function(z) grepl(z, text, fixed = TRUE), logical(1)))
  list(
    pass = pass,
    detail = paste(names(x), collapse = " | ")
  )
}
pub_inventory <- if (nzchar(pub_extract$root)) {
  distributed_inventory_ok(pub_extract$root)
} else {
  list(pass = FALSE, detail = "no public root")
}
add_rc(
  "RC028",
  "Public distributed inventory excludes local library/package paths",
  isTRUE(pub_inventory$pass),
  pub_inventory$detail,
  "sanitised schema without local paths"
)

run_public_package_self_test <- function(root, out_dir) {
  result <- list(
    status = 1L,
    log = "",
    static = data.frame(),
    scientific = data.frame(),
    integration = data.frame()
  )
  if (!nzchar(root) || !dir.exists(root)) return(result)
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  if (!file.exists(rscript)) return(result)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- normalizePath(
    file.path(out_dir, "PUBLIC_PACKAGE_SELF_TEST_LOG.txt"),
    winslash = "/",
    mustWork = FALSE
  )
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(root)
  status <- system2(
    rscript,
    c("--vanilla", "VERIFY_PUBLIC_PACKAGE.R"),
    stdout = log_path,
    stderr = log_path
  )
  setwd(old)
  result$status <- as.integer(status)
  result$log <- log_path
  result$static <- read_test_file(
    file.path(root, "outputs", "tests", "static_preflight_tests.csv")
  )
  result$scientific <- read_test_file(
    file.path(root, "outputs", "tests", "offline_scientific_tests.csv")
  )
  result$integration <- read_test_file(
    file.path(root, "outputs", "tests", "integration_smoke_tests.csv")
  )
  for (f in c(
    "static_preflight_tests.csv",
    "offline_scientific_tests.csv",
    "integration_smoke_tests.csv"
  )) {
    source_path <- file.path(root, "outputs", "tests", f)
    if (file.exists(source_path)) {
      file.copy(source_path, file.path(out_dir, f), overwrite = TRUE)
    }
  }
  result
}

out_dir <- file.path("outputs", "release_candidates")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
public_self_test_dir <- file.path(out_dir, "public_package_self_test")
public_self_test <- run_public_package_self_test(
  pub_extract$root,
  public_self_test_dir
)
public_self_test_pass <- identical(public_self_test$status, 0L) &&
  nrow(public_self_test$static) == 208L &&
  all(public_self_test$static$status == "PASS") &&
  nrow(public_self_test$scientific) == 108L &&
  all(public_self_test$scientific$status == "PASS") &&
  nrow(public_self_test$integration) == 84L &&
  all(public_self_test$integration$status == "PASS")
add_rc(
  "RC029",
  "Extracted public package passes its own syntax, preflight, scientific, and integration workflow",
  public_self_test_pass,
  paste0(
    "exit=", public_self_test$status,
    "; static=", sum(public_self_test$static$status == "PASS"), "/", nrow(public_self_test$static),
    "; scientific=", sum(public_self_test$scientific$status == "PASS"), "/", nrow(public_self_test$scientific),
    "; integration=", sum(public_self_test$integration$status == "PASS"), "/", nrow(public_self_test$integration)
  ),
  "exit=0; static=208/208; scientific=108/108; integration=84/84"
)

review_required <- c(
  file.path("data", "restricted", "systems_registry_159_restricted.csv"),
  file.path("data", "audit", "territorial_selector_audit_final_v1.csv"),
  file.path("data", "audit", "territorial_name_ambiguity_audit_v1.csv")
)
review_required_present <- nzchar(rev_extract$root) &&
  all(file.exists(file.path(rev_extract$root, review_required)))
review_registry_n <- if (
  nzchar(rev_extract$root) &&
    file.exists(file.path(rev_extract$root, review_required[[1]]))
) {
  nrow(as.data.frame(
    read_csv_utf8(file.path(rev_extract$root, review_required[[1]])),
    stringsAsFactors = FALSE
  ))
} else {
  0L
}
add_rc(
  "RC030",
  "Reviewer-controlled package retains the processed 159-system registry and detailed audit",
  review_required_present && review_registry_n == 159L,
  paste0(
    "files=", paste(file.exists(file.path(rev_extract$root, review_required)), collapse = "|"),
    "; registry_rows=", review_registry_n
  ),
  "TRUE|TRUE|TRUE; registry_rows=159"
)

rev_inventory <- if (nzchar(rev_extract$root)) {
  distributed_inventory_ok(rev_extract$root)
} else {
  list(pass = FALSE, detail = "no reviewer root")
}
add_rc(
  "RC031",
  "Reviewer distributed inventory excludes author-workstation library/package paths",
  isTRUE(rev_inventory$pass),
  rev_inventory$detail,
  "sanitised schema without local paths"
)

privacy_report <- data.frame(
  check = c(
    "forbidden_public_files",
    "safe_public_assets",
    "restricted_coordinate_fingerprints",
    "restricted_record_label_fingerprints",
    "public_inventory_sanitised",
    "public_self_test",
    "reviewer_controlled_assets",
    "reviewer_inventory_sanitised"
  ),
  status = c(
    ifelse(length(forbidden_present) == 0L, "PASS", "FAIL"),
    ifelse(safe_public_present, "PASS", "FAIL"),
    ifelse(nrow(coordinate_hits) == 0L, "PASS", "FAIL"),
    ifelse(nrow(record_label_hits) == 0L, "PASS", "FAIL"),
    ifelse(isTRUE(pub_inventory$pass), "PASS", "FAIL"),
    ifelse(public_self_test_pass, "PASS", "FAIL"),
    ifelse(review_required_present && review_registry_n == 159L, "PASS", "FAIL"),
    ifelse(isTRUE(rev_inventory$pass), "PASS", "FAIL")
  ),
  detail = c(
    paste(forbidden_present, collapse = " | "),
    paste(safe_public_files, collapse = " | "),
    if (nrow(coordinate_hits)) {
      paste(coordinate_hits$file, coordinate_hits$source_row, sep = "#", collapse = " | ")
    } else {
      "none"
    },
    if (nrow(record_label_hits)) {
      paste(record_label_hits$file, record_label_hits$public_id, record_label_hits$field, record_label_hits$value, sep = "#", collapse = " | ")
    } else {
      "none"
    },
    pub_inventory$detail,
    paste0("exit=", public_self_test$status),
    paste(review_required, collapse = " | "),
    rev_inventory$detail
  ),
  stringsAsFactors = FALSE
)
write_csv_utf8(
  privacy_report,
  file.path(out_dir, "RC6_PUBLIC_PACKAGE_PRIVACY_REPORT.csv")
)

if (nzchar(pub_extract$temp)) unlink(pub_extract$temp, recursive = TRUE, force = TRUE)
if (nzchar(rev_extract$temp)) unlink(rev_extract$temp, recursive = TRUE, force = TRUE)

all_checks <- dplyr::bind_rows(checks)
write_csv_utf8(
  all_checks,
  file.path(out_dir, "RC6_RELEASE_CANDIDATE_TESTS.csv")
)
status <- if (all(all_checks$status == "PASS")) "PASS" else "FAIL"
writeLines(
  c(
    paste0(
      "RiegoAzuay DSS v1.0.0-rc6 release-candidate gate: ",
      status
    ),
    paste0(
      "Checks passed: ",
      sum(all_checks$status == "PASS"),
      " / ",
      nrow(all_checks)
    ),
    "Boundary: PASS authorises review of the candidate; it does not create an immutable repository release, public licence, or DOI."
  ),
  file.path(out_dir, "RC6_RELEASE_CANDIDATE_STATUS.txt"),
  useBytes = TRUE
)

staging <- tempfile("rc6_evidence_")
dir.create(staging, recursive = TRUE)
files_to_copy <- c(
  file.path(out_dir, "RC6_RELEASE_CANDIDATE_TESTS.csv"),
  file.path(out_dir, "RC6_RELEASE_CANDIDATE_STATUS.txt"),
  file.path(out_dir, "RC6_PUBLIC_PACKAGE_PRIVACY_REPORT.csv"),
  index_path,
  public_path,
  paste0(public_path, "_SHA256.txt"),
  review_path,
  paste0(review_path, "_SHA256.txt"),
  "renv.lock",
  file.path("reproducibility", "sessionInfo.txt"),
  file.path("reproducibility", "installed_package_versions.csv"),
  file.path("reproducibility", "ENVIRONMENT_FREEZE_METADATA.csv"),
  file.path("reproducibility", "ENVIRONMENT_FREEZE_STATUS.txt"),
  file.path("reproducibility", "environment_freeze_tests.csv"),
  file.path("reproducibility", "REPRODUCIBILITY_MANIFEST_SHA256.csv"),
  current_static_path,
  current_scientific_path,
  current_integration_path,
  file.path(public_self_test_dir, "PUBLIC_PACKAGE_SELF_TEST_LOG.txt"),
  file.path(public_self_test_dir, "static_preflight_tests.csv"),
  file.path(public_self_test_dir, "offline_scientific_tests.csv"),
  file.path(public_self_test_dir, "integration_smoke_tests.csv"),
  pg_zip,
  pg_sha_file,
  live_zip,
  live_sha_file,
  file.path("docs", "RC4_METHODS_DATA_GOVERNANCE_NOTE.md"),
  file.path("docs", "PATCH_RC4_PUBLIC_PACKAGE_CONFIDENTIALITY.md"),
  file.path("docs", "PATCH_RC5_PUBLIC_INTEGRATION_SOIL_SOURCE_CONTRACT.md"),
  file.path("docs", "PATCH_RC6_PUBLIC_RECORD_LABEL_BOUNDARY.md"),
  file.path("docs", "PATCH_RC3_INSTALLED_PACKAGE_INVENTORY.md"),
  "LICENSE_PENDING.md"
)
files_to_copy <- unique(
  files_to_copy[nzchar(files_to_copy) & file.exists(files_to_copy)]
)
for (f in files_to_copy) {
  rel <- basename(f)
  target <- file.path(staging, rel)
  if (file.exists(target)) {
    target <- file.path(
      staging,
      paste0(
        tools::file_path_sans_ext(rel),
        "_copy",
        if (nzchar(tools::file_ext(rel))) {
          paste0(".", tools::file_ext(rel))
        } else {
          ""
        }
      )
    )
  }
  file.copy(f, target, overwrite = TRUE)
}
write_sha256_manifest(staging)
verify_sha256_manifest(staging)

evidence_zip <- file.path(
  out_dir,
  "RiegoAzuay-DSS-v1.0.0-rc6_FREEZE_EVIDENCE.zip"
)
evidence_abs <- write_verified_zip_app(staging, evidence_zip)
evidence_sha <- hash_file_sha256(evidence_abs)
writeLines(
  c(
    paste0(evidence_sha, "  ", basename(evidence_abs)),
    paste0("RC6 gate status: ", status),
    paste0(
      "Checks passed: ",
      sum(all_checks$status == "PASS"),
      " / ",
      nrow(all_checks)
    )
  ),
  paste0(evidence_abs, "_SHA256.txt"),
  useBytes = TRUE
)
unlink(staging, recursive = TRUE, force = TRUE)

print_test_summary(
  all_checks,
  "RESULTADO GENERAL: LA CANDIDATA RC6 REGISTRÓ FALLOS."
)
cat("RESULTADO GENERAL: CANDIDATA RC6 PREPARADA PARA REVISIÓN.\n")
cat(
  "Evidence bundle: ",
  normalizePath(evidence_abs, winslash = "/", mustWork = TRUE),
  "\n",
  sep = ""
)
