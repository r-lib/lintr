test_that("function_argument_linter returns the correct linting", {
  linter <- function_argument_linter()
  msg <- rex::rex("Arguments without defaults should come before arguments with defaults.")

  expect_lint("function(x, y = 1, z) {}", list(message = msg, column_number = 20L), linter)
  expect_lint("function(x, y = 1, z, w = 2) {}", list(message = msg, column_number = 20L), linter)
  expect_lint("function(x, y = 1, z = 2) {}", NULL, linter)
  expect_lint("function(x, y, z = 1) {}", NULL, linter)

  # ... handled correctly
  expect_lint("function(x, y, z = 1, ..., w) {}", list(message = msg, column_number = 28L), linter)
  expect_lint("function(x, y, z = 1, ...) {}", NULL, linter)
  expect_lint("function(x, y, ..., z = 1) {}", NULL, linter)

  # Multi-line also works
  expect_lint(
    trim_some("
      function(x,
               y = 1,
               z) {
      }
    "),
    list(message = msg, line_number = 3L),
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
    list(message = msg, line_number = 4L),
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
  msg <- rex::rex("Arguments without defaults should come before arguments with defaults.")

  expect_lint("\\(x, y = 1, z) {}", list(message = msg, column_number = 13L), linter)
  expect_lint("\\(x, y = 1, z, w = 2) {}", list(message = msg, column_number = 13L), linter)
  expect_lint("\\(x, y = 1, z = 2) {}", NULL, linter)
  expect_lint("\\(x, y, z = 1) {}", NULL, linter)
})
