test_that("returns the correct linting", {
  linter <- trailing_whitespace_linter()

  expect_lint("blah", NULL, linter)

  expect_lint(
    "blah <- 1  ",
    list(message = rex("Trailing whitespace is superfluous."), column_number = 10L),
    linter
  )

  expect_lint(
    "blah <- 1  \n'hi'",
    rex("Trailing whitespace is superfluous."),
    linter
  )

  expect_lint(
    "blah <- 1\n'hi'\na <- 2  ",
    list(message = rex("Trailing whitespace is superfluous."), line_number = 3L),
    linter
  )
})

test_that("also handles completely empty lines per allow_empty_lines argument", {
  linter <- trailing_whitespace_linter()

  expect_lint(
    "blah <- 1\n  \n'hi'\na <- 2",
    list(message = rex("Trailing whitespace is superfluous."), line_number = 2L),
    linter
  )

  expect_lint(
    "blah <- 1  ",
    list(message = rex("Trailing whitespace is superfluous."), column_number = 10L),
    trailing_whitespace_linter(allow_empty_lines = TRUE)
  )

  expect_lint(
    "blah <- 1\n  \n'hi'\na <- 2",
    NULL,
    trailing_whitespace_linter(allow_empty_lines = TRUE)
  )
})

test_that("also handles trailing whitespace in string constants", {
  linter <- trailing_whitespace_linter()
  msg <- rex::rex("Trailing whitespace is superfluous.")

  expect_lint("blah <- '  \n  \n'", NULL, linter)
  # Don't exclude past the end of string
  expect_lint(
    "blah <- '  \n  \n'  ",
    list(message = msg, line_number = 3L),
    linter
  )
  # can be enabled with allow_in_strings = FALSE
  expect_lint(
    "blah <- '  \n  \n'",
    list(message = msg, line_number = 1L),
    trailing_whitespace_linter(allow_empty_lines = TRUE, allow_in_strings = FALSE)
  )
  expect_lint(
    "blah <- '  \n  \n'",
    list(
      list(message = msg, line_number = 1L),
      list(message = msg, line_number = 2L)
    ),
    trailing_whitespace_linter(allow_in_strings = FALSE)
  )
})
