context("trailing_blank_lines_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    trailing_blank_lines_linter)

  expect_lint("blah <- 1  ",
    NULL,
    trailing_blank_lines_linter)

  expect_lint("blah <- 1\nblah",
    NULL,
    trailing_blank_lines_linter)

  expect_lint("blah <- 1\nblah\n \n blah",
    NULL,
    trailing_blank_lines_linter)

  expect_lint("blah <- 1\n",
    rex("Trailing blank lines are superfluous."),
    trailing_blank_lines_linter)

  expect_lint("blah <- 1\n  ",
    rex("Trailing blank lines are superfluous."),
    trailing_blank_lines_linter)

  expect_lint("blah <- 1\n \n ",
    list(
      rex("Trailing blank lines are superfluous."),
      rex("Trailing blank lines are superfluous.")
      ),
    trailing_blank_lines_linter)

  expect_lint("blah <- 1\n\n",
    list(
      rex("Trailing blank lines are superfluous."),
      rex("Trailing blank lines are superfluous.")
      ),
    trailing_blank_lines_linter)

  expect_lint("blah <- 1\n\t\n",
    list(
      rex("Trailing blank lines are superfluous."),
      rex("Trailing blank lines are superfluous.")
      ),
    trailing_blank_lines_linter)
})
