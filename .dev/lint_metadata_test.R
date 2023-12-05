# This script is designed to find linters that lack metadata tests.
#   To do so, it forces xml_nodes_to_lints() to give the wrong information,
#   runs the test suite, and finds linters that nevertheless pass all their tests.
library(testthat)

xml_nodes_to_lints_file <- "R/xml_nodes_to_lints.R"

original <- readLines(xml_nodes_to_lints_file)
writeLines(
  sub(
    "line_number = as.integer(line1)",
    "line_number = as.integer(2^31 - 1)",
    original,
    fixed = TRUE
  ),
  xml_nodes_to_lints_file
)
# Not useful in CI but good when running locally.
withr::defer(writeLines(original, xml_nodes_to_lints_file))

pkgload::load_all()

report <- test_dir(
  "tests/testthat",
  filter = "linter$",
  stop_on_failure = FALSE,
  reporter = SilentReporter$new()
)
names(report) <- vapply(report, `[[`, "file", FUN.VALUE = character(1L))

# Hack the nested structure of the testthat report to identify which files have
#   any failed test
failed <- report |>
  vapply(
    \(x) any(vapply(x$results, inherits, "expectation_failure", FUN.VALUE = logical(1L))),
    logical(1L)
  ) |>
  which() |>
  names() |>
  unique() |>
  gsub(pattern = "^test-|\\.R$", replacement = "")

passed <- setdiff(
  available_linters(tags = NULL)$linter,
  failed
)

if (length(passed) > 0L) {
  stop("Please add tests of lint metadata for the following linters: ", toString(passed))
}
