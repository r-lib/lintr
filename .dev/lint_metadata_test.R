# This script is designed to find linters that lack metadata tests.
#   To do so, it forces xml_nodes_to_lints() to give the wrong information,
#   runs the test suite, and finds linters that nevertheless pass all their tests.
library(testthat)

line_replacements <- paste("line_number =", c(
  `R/xml_nodes_to_lints.R` = "as.integer(line1)",
  `R/path_utils.R` = 'token[["line1"]]'
))

originals <- lapply(names(line_replacements), readLines)

# If the code is refactored, this script may need to be updated.
has_expected <- mapply(
  function(lines, pattern) any(grepl(pattern, lines, fixed = TRUE)),
  originals, line_replacements
)
if (!all(has_expected)) {
  stop(
    "Please update this workflow -- expected to find these strings in these files:\n",
    paste0(names(line_replacements)[!has_expected], ": ", line_replacements[!has_expected], collapse = "\n")
  )
}

for (ii in seq_along(line_replacements)) {
  writeLines(
    sub(line_replacements[ii], "line_number = as.integer(2^31 - 1)", originals[[ii]], fixed = TRUE),
    names(line_replacements[ii])
  )
}

# Not useful in CI but good when running locally.
withr::defer({
  for (ii in seq_along(line_replacements)) writeLines(originals[[ii]], names(line_replacements[ii]))
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
  stop("Please add tests of lint metadata for the following linters: ", toString(passed))
}
