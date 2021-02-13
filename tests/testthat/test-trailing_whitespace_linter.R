test_that("returns the correct linting", {
  linter <- trailing_whitespace_linter()

  expect_lint("blah", NULL, linter)

  expect_lint(
    "blah <- 1  ",
    list(message = rex("Trailing whitespace is superfluous."), column_number = 10),
    linter
  )

  expect_lint(
    "blah <- 1  \n'hi'",
    rex("Trailing whitespace is superfluous."),
    linter
  )

  expect_lint(
    "blah <- 1\n'hi'\na <- 2  ",
    list(message = rex("Trailing whitespace is superfluous."), line_number = 3),
    linter
  )
})
