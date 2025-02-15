# here are some extraneous variables that are not part of the config directly

non_default_linter <- any_duplicated_linter()
attr(non_default_linter, "name") <- "any_duplicated_linter"
excluded_files <- "tests/testthat.R"

linters <- linters_with_defaults(
  non_default_linter,
  assignment_linter = NULL
)
exclude <- "# NOLINT"
exclusions <- list(excluded_files)
