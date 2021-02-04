test_that("returns the correct linting", {
  linter <- trailing_blank_lines_linter()
  msg <- rex("Trailing blank lines are superfluous.")

  expect_lint("blah", NULL, linter)
  expect_lint("blah <- 1  ", NULL, linter)
  expect_lint("blah <- 1\nblah", NULL, linter)
  expect_lint("blah <- 1\nblah\n \n blah", NULL, linter)
  expect_lint("blah <- 1\n", msg, linter)
  expect_lint("blah <- 1\n  ", msg, linter)

  expect_lint(
    "blah <- 1\n \n ",
    list(
      msg,
      msg
    ),
    linter
  )

  expect_lint(
    "blah <- 1\n\n",
    list(
      msg,
      msg
    ),
    linter
  )

  expect_lint(
    "blah <- 1\n\t\n",
    list(
      msg,
      msg
    ),
    linter
  )
})
