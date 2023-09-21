linters <- linters_with_defaults(
  any_duplicated_linter(),
  assignment_linter = NULL
)
exclusions <- list("tests/testthat.R")
