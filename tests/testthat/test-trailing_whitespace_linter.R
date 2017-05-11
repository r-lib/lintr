context("trailing_whitespace_linter")

test_that("returns the correct linting", {
  msg <- rex("Trailing whitespace is superfluous.")
  linter <- trailing_whitespace_linter()
  expect_is(linter, "linter")

  expect_lint("blah", NULL, linter)

  expect_lint("blah <- 1  ", c(message = msg, column_number = 10), linter)

  expect_lint("blah <- 1  \n'hi'", msg, linter)

  expect_lint("blah <- 1\n'hi'\na <- 2  ",
    list(c(message = msg, line_number = 3)),
    linter)
})
