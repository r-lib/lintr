library(testthat)
test_that("test repeat_linter", {
  linter <- repeat_linter()
  msg <- rex::rex("Use 'repeat' instead of 'while (TRUE)' for infinite loops.")

  expect_lint("repeat { }", NULL, linter)
  expect_lint("while (FALSE) { }", NULL, linter)
  expect_lint("while (i < 5) { }", NULL, linter)
  expect_lint("while (j < 5) TRUE", NULL, linter)
  expect_lint("while (TRUE && j < 5) { ... }", NULL, linter)

  expect_lint("while (TRUE) { }", msg, linter)
  expect_lint("`while`(TRUE)", msg, linter)
  expect_lint("for (i in 1:10) { while (TRUE) { if (i == 5) { break } } }", msg, linter)
  expect_lint("while (TRUE) { while (TRUE) { } }", list(msg, msg), linter)
})
