r_syntax_files <- sort(unique(c(
  list.files(".", pattern = "\\.R$", full.names = TRUE),
  list.files("R", pattern = "\\.R$", full.names = TRUE)
)))

r_syntax_failures <- list()
for (r_syntax_file in r_syntax_files) {
  r_syntax_error <- tryCatch(
    {
      parse(file = r_syntax_file, encoding = "UTF-8", keep.source = TRUE)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (!is.null(r_syntax_error)) {
    r_syntax_failures[[r_syntax_file]] <- r_syntax_error
  }
}

if (length(r_syntax_failures)) {
  cat("R syntax gate failed before package installation or test execution.\n")
  for (r_syntax_file in names(r_syntax_failures)) {
    cat(" - ", r_syntax_file, ": ", r_syntax_failures[[r_syntax_file]], "\n", sep = "")
  }
  stop("Correct the R syntax errors listed above before continuing.", call. = FALSE)
}

cat(sprintf("R syntax gate passed: %d / %d files.\n", length(r_syntax_files), length(r_syntax_files)))
rm(r_syntax_file, r_syntax_error, r_syntax_failures, r_syntax_files)
