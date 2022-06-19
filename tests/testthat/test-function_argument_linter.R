test_that("function_argument_linter returns the correct linting", {
  linter <- function_argument_linter()
  msg <- rex::rex("Arguments without defaults should come before arguments with defaults.")

  expect_lint("function(x, y = 1, z) {}", list(message = msg, column_number = 20L), linter)
  expect_lint("function(x, y = 1, z, w = 2) {}", list(message = msg, column_number = 20L), linter)
  expect_lint("function(x, y = 1, z = 2) {}", NULL, linter)
  expect_lint("function(x, y, z = 1) {}", NULL, linter)
})
