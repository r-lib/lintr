context("trailing_blank_lines_linter")

test_that("returns the correct linting", {
  msg <- rex("Trailing blank lines are superfluous.")
  linter <- trailing_blank_lines_linter()
  expect_is(linter, "linter")

  expect_lint("blah", NULL, linter)

  expect_lint("blah <- 1  ", NULL, linter)

  expect_lint("blah <- 1\nblah", NULL, linter)

  expect_lint("blah <- 1\nblah\n \n blah", NULL, linter)

  expect_lint("blah <- 1\n", msg, linter)

  expect_lint("blah <- 1\n  ", msg, linter)

  expect_lint("blah <- 1\n \n ", list(msg, msg), linter)

  expect_lint("blah <- 1\n\n", list(msg, msg), linter)

  expect_lint("blah <- 1\n\t\n", list(msg, msg), linter)
})
