test_that("returns the correct linting", {
  linter <- object_length_linter()
  lint_msg <- rex::rex("Variable and function names should not be longer than 30 characters.")

  expect_no_lint("blah", linter)

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal <- 1", lint_msg, linter)

  expect_lint(
    "very_very_very_very_long_variable_names_are_not_ideal <<- 'foo'",
    rex::rex("Variable and function names should not be longer than 40 characters."),
    object_length_linter(length = 40L)
  )
})

# Regression tests for #871
test_that("lints S3 generics correctly", {
  linter <- object_length_linter()
  lint_msg <- rex::rex("Variable and function names should not be longer than 30 characters.")

  expect_no_lint("print.very_very_long_class_name <- 1", linter)
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
    lint_package(
      test_path("dummy_packages", "missing_dep"),
      linters = object_length_linter(),
      parse_settings = FALSE
    ),
    3L
  )
})

test_that("object_length_linter won't fail if dependency has no exports", {
  expect_length(
    lint_package(
      test_path("dummy_packages", "no_export_dep"),
      linters = object_length_linter(),
      parse_settings = FALSE
    ),
    1L
  )
})

test_that("function shorthand is caught", {
  skip_if_not_r_version("4.1.0")

  expect_lint(
    "abcdefghijklm <- \\() NULL",
    "function names",
    object_length_linter(length = 10L)
  )
})

test_that("rlang name injection is handled", {
  linter <- object_length_linter(length = 10L)

  expect_no_lint("tibble('{foo() |> bar() |> baz()}' := TRUE)", linter)
  expect_lint("DT[, 'a_very_long_name' := FALSE]", "names should not be longer than 10 characters", linter)
})

test_that("literals in assign() and setGeneric() are checked", {
  linter <- object_length_linter(length = 10L)
  lint_msg <- rex::rex("Variable and function names should not be longer")

  expect_no_lint("assign('good_name', 2, env)", linter)
  expect_no_lint("assign('good_name', 'badBadBadBadName', env)", linter)
  expect_lint("assign('badBadBadBadName', 2, env)", lint_msg, linter)
  expect_lint('assign("badBadBadBadName", 2, env)', lint_msg, linter)

  expect_no_lint("setGeneric('good_name', function(x) x)", linter)
  expect_no_lint("setGeneric('good_name', function(x) x, package = 'badBadBadBadName')", linter)
  expect_lint("setGeneric('badBadBadBadName', function(x) x)", lint_msg, linter)
  expect_lint('setGeneric("badBadBadBadName", function(x) x)', lint_msg, linter)

  # Ditto for keyword arguments
  expect_no_lint("assign(envir = 'badBadBadBadName', nm, 2)", linter)
  expect_no_lint("assign(envir = 'badBadBadBadName', 'good_name', 2)", linter)
  expect_no_lint("assign(envir = 'badBadBadBadName', x = 'good_name', 2)", linter)
  expect_lint("assign(envir = 'good_env_name', 'badBadBadBadName', 2)", lint_msg, linter)
  expect_lint("assign(envir = 'good_env_name', x = 'badBadBadBadName', 2)", lint_msg, linter)
})
