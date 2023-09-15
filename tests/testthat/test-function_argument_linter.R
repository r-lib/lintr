test_that("function_argument_linter skips allowed usages", {
  linter <- function_argument_linter()

  expect_lint("function(x, y = 1, z = 2) {}", NULL, linter)
  expect_lint("function(x, y, z = 1) {}", NULL, linter)

  # ... handled correctly
  expect_lint("function(x, y, z = 1, ...) {}", NULL, linter)
  expect_lint("function(x, y, ..., z = 1) {}", NULL, linter)
})

test_that("function_argument_linter blocks disallowed usages", {
  linter <- function_argument_linter()
  lint_msg <- rex::rex("Arguments without defaults should come before arguments with defaults.")

  expect_lint("function(x, y = 1, z) {}", list(message = lint_msg, column_number = 20L), linter)
  expect_lint("function(x, y = 1, z, w = 2) {}", list(message = lint_msg, column_number = 20L), linter)

  expect_lint("function(x, y, z = 1, ..., w) {}", list(message = lint_msg, column_number = 28L), linter)

  # Multi-line also works
  expect_lint(
    trim_some("
      function(x,
               y = 1,
               z) {
      }
    "),
    list(message = lint_msg, line_number = 3L),
    linter
  )
  expect_lint(
    trim_some("
      function(x,
               y # comment for distraction
               = 1,
               z,
               w = 2) {
      }
    "),
    list(message = lint_msg, line_number = 4L),
    linter
  )
  expect_lint(
    trim_some("
      function(x,
               y = 1,
               z = 2) {
      }
    "),
    NULL,
    linter
  )
})

test_that("function_argument_linter also lints lambda expressions", {
  skip_if_not_r_version("4.1.0")
  linter <- function_argument_linter()
  lint_msg <- rex::rex("Arguments without defaults should come before arguments with defaults.")

  expect_lint("\\(x, y = 1, z) {}", list(message = lint_msg, column_number = 13L), linter)
  expect_lint("\\(x, y = 1, z, w = 2) {}", list(message = lint_msg, column_number = 13L), linter)
  expect_lint("\\(x, y = 1, z = 2) {}", NULL, linter)
  expect_lint("\\(x, y, z = 1) {}", NULL, linter)
})

test_that("Use of missing() is reported in the lint message", {
  linter <- function_argument_linter()
  simple_msg <- "Arguments without defaults should come before arguments with defaults."

  expect_lint(
    trim_some("
      function(x, y = 1, z) {
        if (missing(z)) {
          z <- 2
        }
      }
    "),
    rex::rex(simple_msg, anything, "missing()"),
    linter
  )

  expect_lint(
    trim_some("
      function(x, y = 1, z) {
        if (y > 1) {
          if (missing(z)) {
            z <- 2
          }
        }
      }
    "),
    rex::rex(simple_msg, anything, "missing()"),
    linter
  )

  # inline function
  expect_lint(
    "function(x = 2, y) if (missing(y)) x else y",
    rex::rex(simple_msg, anything, "missing()"),
    linter
  )

  # missing() used for a different argument
  expect_lint(
    trim_some("
      function(x, y = 1, z) {
        if (missing(x)) {
          z <- 2
        }
      }
    "),
    rex::rex(simple_msg, end),
    linter
  )

  # missing() used in the signature (not quite the same, and easier to spot)
  expect_lint(
    trim_some("
      function(x = 1, y, z = missing(y)) {
        x
      }
    "),
    rex::rex(simple_msg, end),
    linter
  )
})

test_that("multiple lints give correct message", {
  simple_msg <- "Arguments without defaults should come before arguments with defaults."

  expect_lint(
    trim_some("{
      function(x, y = 1, z) {
        x
      }
      function(x, y = 1, z) {
        if (missing(z)) {
          z <- 2
        }
      }
      function(x, y = 1, z, w) {
        if (missing(z)) {
          z <- 2
        }
      }
    }"),
    list(
      list(rex::rex(simple_msg, end), line_number = 2L),
      list(rex::rex(simple_msg, anything, "missing()"), line_number = 5L),
      list(rex::rex(simple_msg, anything, "missing()"), line_number = 10L, column_number = 22L),
      list(rex::rex(simple_msg, end), line_number = 10L, column_number = 25L)
    ),
    function_argument_linter()
  )
})
