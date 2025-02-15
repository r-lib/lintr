linters <- linters_with_defaults(
  expect_null_linter(),
  assignment_linter = NULL
)
exclude <- "# SKIP_LINT"
exclusions <- list("R/lint_me.R")
