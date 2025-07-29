# This script is designed to find linters that lack metadata tests.
#   To do so, it forces Lint() to give the wrong information,
#   runs the test suite, and finds linters that nevertheless pass all their tests.
library(testthat)

lint_file <- "R/lint.R"

original <- readLines(lint_file)
expected_line <- "line_number = as.integer(line_number)"
if (sum(grepl(expected_line, original, fixed = TRUE)) != 1L) {
  stop(sprintf(
    "Please update this workflow -- need exactly one hit for line '%s' in file '%s'.",
    expected_line, lint_file
  ))
}
writeLines(
  sub(expected_line, "line_number = as.integer(2^31 - 1)", original, fixed = TRUE),
  lint_file
)
# Not useful in CI but good when running locally.
withr::defer({
  writeLines(original, lint_file)
  pkgload::load_all()
})

pkgload::load_all()

report <- test_dir(
  "tests/testthat",
  filter = "linter$",
  stop_on_failure = FALSE,
  reporter = SilentReporter$new()
)
names(report) <- gsub("^test-|\\.R$", "", vapply(report, `[[`, "file", FUN.VALUE = character(1L)))

# Hack the nested structure of the testthat report to identify which files have
#   any failed test
failed <- report |>
  vapply(
    \(x) any(vapply(x$results, inherits, "expectation_failure", FUN.VALUE = logical(1L))),
    logical(1L)
  ) |>
  which() |>
  names() |>
  unique()

passed <- setdiff(
  available_linters(tags = NULL)$linter,
  failed
)

if (length(passed) > 0L) {
  # Extra logging to possibly help debug
  cat(sprintf("Executed the following test files: %s\n", toString(failed)))
  stop("Please add tests of lint metadata for the following linters: ", toString(passed))
}
