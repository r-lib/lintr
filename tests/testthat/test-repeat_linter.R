library(testthat)
test_that("test repeat_linter", {
  linter <- repeat_linter()
  msg <- rex::rex("'while (TRUE)' is not recommended for infinite loops. Use 'repeat' instead.")

  expect_lint("repeat { }", NULL, linter)
  expect_lint("while (FALSE) { }", NULL, linter)
  expect_lint("while (i < 5) { }", NULL, linter)

  expect_lint("while (TRUE) { }", msg, linter)
  expect_lint("for (i in 1:10) { while (TRUE) { if (i == 5) { break } } }", msg, linter)
  expect_lint("while (TRUE) { while (TRUE) { } }", list(msg, msg), linter)
})
