test_that("returns the correct linting", {

  linter <- object_length_linter()
  lint_msg <- rex("Variable and function names should not be longer than 30 characters.")

  expect_lint("blah", NULL, linter)

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal <- 1", lint_msg, linter)

  expect_lint(
    "very_very_very_very_long_variable_names_are_not_ideal <<- 'foo'",
    rex("Variable and function names should not be longer than 40 characters."),
    object_length_linter(length = 40)
  )

  # Regression tests for #871

  expect_lint("print.very_very_long_class_name <- 1", NULL, linter)
  expect_lint("print.very_very_very_very_long_class_name <- 1", lint_msg, linter)

  expect_lint(trim_some("
    very_very_very_long_generic_name <- function(x, ...) {
      UseMethod(\"very_very_very_long_generic_name\")
    }

    very_very_very_long_generic_name.short_class <- function(x, ...) {
      42L
    }

    very_very_very_long_generic_name.very_very_very_very_long_class_name <- function(x, ...) {
      2L
    }
  "), list(
    list(line_number = 1),
    list(line_number = 9)
  ), linter)
})
