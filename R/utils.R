`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

clamp_value <- function(x, lower, upper) {
  pmax(lower, pmin(upper, x))
}

read_csv_utf8 <- function(path, col_types = NULL, diagnostic_dir = file.path("outputs", "diagnostics")) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)
  x <- suppressWarnings(readr::read_csv(
    path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    na = c("", "NA", "N/A", "null", "NULL"),
    col_types = col_types,
    progress = FALSE,
    name_repair = "check_unique"
  ))
  issues <- readr::problems(x)
  if (nrow(issues) > 0L) {
    issues$source_file <- normalizePath(path, winslash = "/", mustWork = FALSE)
    diag_path <- file.path(diagnostic_dir, paste0("CSV_PARSE_PROBLEMS_", tools::file_path_sans_ext(basename(path)), ".csv"))
    readr::write_excel_csv(issues, diag_path, na = "")
    stop(
      "CSV parsing failed for ", path, ". Diagnostic: ", diag_path,
      call. = FALSE
    )
  }
  x
}

read_key_value_config <- function(path) {
  x <- as.data.frame(read_csv_utf8(path), stringsAsFactors = FALSE)
  required <- c("parameter", "value")
  if (!all(required %in% names(x))) {
    stop("Key-value config requires parameter and value columns: ", path, call. = FALSE)
  }
  keys <- trimws(as.character(x$parameter))
  if (any(is.na(keys) | !nzchar(keys))) {
    stop("Key-value config contains blank parameter names: ", path, call. = FALSE)
  }
  if (anyDuplicated(keys)) {
    duplicates <- unique(keys[duplicated(keys)])
    stop("Key-value config contains duplicated parameters: ", paste(duplicates, collapse = ", "), call. = FALSE)
  }
  as.list(stats::setNames(as.character(x$value), keys))
}

write_csv_utf8 <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_excel_csv(x, path, na = "")
  invisible(path)
}

ensure_output_dirs <- function() {
  dirs <- c("outputs/results", "outputs/figures", "outputs/reports", "outputs/bundles")
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

new_test_row <- function(test_id, description, pass, observed, expected) {
  data.frame(
    test_id = as.character(test_id),
    description = as.character(description),
    status = ifelse(isTRUE(pass), "PASS", "FAIL"),
    observed = paste(observed, collapse = " | "),
    expected = paste(expected, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

print_test_summary <- function(tests, fail_message = "RESULTADO GENERAL: SE REGISTRARON FALLOS.") {
  passed <- sum(tests$status == "PASS")
  total <- nrow(tests)
  cat("\nPassed:", passed, "/", total, "\n")
  if (passed != total) {
    print(tests[tests$status != "PASS", , drop = FALSE])
    stop(fail_message, call. = FALSE)
  }
  invisible(tests)
}

parse_bool <- function(x, default = FALSE) {
  if (length(x) == 0L || is.na(x) || !nzchar(trimws(as.character(x)))) return(default)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y", "si", "sí")
}

numeric_or_na <- function(x) {
  suppressWarnings(as.numeric(x))
}

text_present <- function(x) {
  length(x) > 0L && !is.na(x) && nzchar(trimws(as.character(x)))
}

parse_named_numeric_map <- function(x) {
  out <- numeric(0)
  if (!text_present(x)) return(out)
  parts <- unlist(strsplit(as.character(x), ";", fixed = TRUE), use.names = FALSE)
  parts <- trimws(parts[nzchar(trimws(parts))])
  for (part in parts) {
    kv <- unlist(strsplit(part, "=", fixed = TRUE), use.names = FALSE)
    if (length(kv) != 2L) stop("Invalid key=value entry: ", part, call. = FALSE)
    val <- suppressWarnings(as.numeric(trimws(kv[[2]])))
    if (!is.finite(val)) stop("Invalid numeric value in entry: ", part, call. = FALSE)
    out[trimws(kv[[1]])] <- val
  }
  out
}

parse_date_vector <- function(x) {
  if (!text_present(x)) return(as.Date(character(0)))
  vals <- trimws(unlist(strsplit(as.character(x), ";", fixed = TRUE), use.names = FALSE))
  vals <- vals[nzchar(vals)]
  as.Date(vals)
}

md5_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(tools::md5sum(path))
}

package_version_string <- function() {
  trimws(readLines("VERSION", warn = FALSE, encoding = "UTF-8")[[1]])
}
