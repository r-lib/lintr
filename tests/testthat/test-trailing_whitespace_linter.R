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

test_that("also handles completely empty lines per allow_empty_lines argument", {
  expect_lint(
    "blah <- 1\n  \n'hi'\na <- 2",
    list(message = rex("Trailing whitespace is superfluous."), line_number = 2),
    linter
  )

  expect_lint(
    "blah <- 1  ",
    list(message = rex("Trailing whitespace is superfluous."), column_number = 10),
    trailing_whitespace_linter(allow_empty_lines = TRUE)
  )

  expect_lint(
    "blah <- 1\n  \n'hi'\na <- 2",
    NULL,
    trailing_whitespace_linter(allow_empty_lines = TRUE)
  )
})
