test_that("linters", {
  # non-empty named list of functions
  x <- default_linters
  expect_type(x, "list")
  expect_gt(length(x), 0L)
  expect_true(all(names(x) != ""))
  expect_true(all(vapply(x, inherits, logical(1L), "linter")))
  expect_true(all(vapply(x, is.function, logical(1L))))
})

test_that("undesirable functions and operators", {
  # non-empty named list of NAs and character strings
  vars <- list(all_undesirable_functions, default_undesirable_functions,
               all_undesirable_operators, default_undesirable_operators)

  for (x in vars) {
    expect_type(x, "list")
    expect_gt(length(x), 0L)
    expect_true(all(names(x) != ""))
    expect_true(all(vapply(x, function(x) is.na(x) || is.character(x), logical(1L))))
    expect_true(all(vapply(x, length, integer(1L)) == 1L))
  }
})

test_that("settings", {
  # non-empty named list
  x <- default_settings
  expect_type(default_settings, "list")
  expect_gt(length(default_settings), 0L)
  expect_true(all(nzchar(names(default_settings))))
})

skip_if_not_installed("patrick")
patrick::with_parameters_test_that(
  "all default linters throw lints with their name on 'default_linter_testcode.R'",
  {
    lints <- lint(
      test_path("default_linter_testcode.R"),
      linters = default_linters[[linter_name]],
      parse_settings = FALSE
    )
    lint_df <- as.data.frame(lints)
    expect_gt(nrow(lint_df), 0L)
    reported_linter_name <- lint_df[["linter"]][1L]
    expect_identical(reported_linter_name, linter_name)
  },
  linter_name = names(default_linters),
  .test_name = names(default_linters)
)
