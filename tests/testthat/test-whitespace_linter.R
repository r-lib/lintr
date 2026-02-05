test_that("whitespace_linter skips allowed usages", {
  linter <- whitespace_linter()

  expect_no_lint("blah", linter)
  expect_no_lint("  blah", linter)
  expect_no_lint("  blah", linter)
  expect_no_lint("#\tblah", linter)
})

test_that("whitespace_linter skips allowed tab usages inside strings", {
  linter <- whitespace_linter()

  expect_no_lint(
    'lint_msg <- "dont flag tabs if\tthey are inside a string."',
    linter
  )

  expect_no_lint(
    'lint_msg <- "dont flag tabs if\n\tthey are inside multiline strings."',
    linter
  )
})

test_that("whitespace_linter blocks disallowed usages", {
  linter <- whitespace_linter()
  lint_msg <- rex::rex("Use spaces to indent, not tabs.")

  expect_lint(
    "\tblah",
    list(message = lint_msg, line_number = 1L, column_number = 1L, ranges = list(c(1L, 1L))),
    linter
  )

  expect_lint(
    "\n\t\t\tblah",
    list(message = lint_msg, line_number = 2L, column_number = 1L),
    linter
  )
})

test_that("whitespace_linter blocks disallowed usages with a pipe", {
  linter <- whitespace_linter()
  lint_msg <- rex::rex("Use spaces to indent, not tabs.")

  expect_lint(
    "a %>%\n\tb()",
    list(message = lint_msg, line_number = 2L, column_number = 1L, ranges = list(c(1L, 1L))),
    linter
  )

  expect_lint(
    "a |>\n\tb()",
    list(message = lint_msg, line_number = 2L, column_number = 1L, ranges = list(c(1L, 1L))),
    linter
  )
})
