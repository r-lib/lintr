test_that("returns the correct linting", {

  linter <- object_length_linter()
  lint_msg <- rex("Variable and function names should not be longer than 30 characters.")

  expect_lint("blah", NULL, linter)

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal <- 1", lint_msg, linter)

  expect_lint(
    "very_very_very_very_long_variable_names_are_not_ideal <<- 'foo'",
    rex("Variable and function names should not be longer than 40 characters."),
    object_length_linter(length = 40L)
  )
})

# Regression tests for #871
test_that("lints S3 generics correctly", {

  linter <- object_length_linter()
  lint_msg <- rex("Variable and function names should not be longer than 30 characters.")

  expect_lint("print.very_very_long_class_name <- 1", NULL, linter)
  expect_lint("print.very_very_very_very_long_class_name <- 1", lint_msg, linter)

  expect_lint(
    trim_some("
      very_very_very_long_generic_name <- function(x, ...) {
        UseMethod(\"very_very_very_long_generic_name\")
      }

      very_very_very_long_generic_name.short_class <- function(x, ...) {
        42L
      }

      very_very_very_long_generic_name.very_very_very_very_long_class_name <- function(x, ...) {
        2L
      }
    "),
    list(
      list(line_number = 1L),
      list(line_number = 9L)
    ),
    linter
  )
})

test_that("object_length_linter won't fail if an imported namespace is unavailable", {
  expect_length(
    lint_package(test_path("dummy_packages", "missing_dep"), linters = object_length_linter(), parse_settings = FALSE),
    3L
  )
})
