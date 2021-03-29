test_that("returns the correct linting", {
  linter <- no_tab_linter()
  msg <- rex("Use spaces to indent, not tabs.")

  expect_lint("blah", NULL, linter)
  expect_lint("  blah", NULL, linter)
  expect_lint("  blah", NULL, linter)
  expect_lint("#\tblah", NULL, linter)

  expect_lint(
    "\tblah",
    list(message = msg, line_number = 1L, column_number = 1, ranges = list(c(1L, 1L))),
    linter
  )

  expect_lint(
    "\n\t\t\tblah",
    list(message = msg, line_number = 2L, column_number = 1L),
    linter
  )

  # ignore strings
  expect_lint(
    'msg <- "dont flag tabs if\tthey are inside a string."',
    NULL,
    linter
  )
  # ignore multi-line strings
  expect_lint(
    'msg <- "dont flag tabs if\n\tthey are inside multiline strings."',
    NULL,
    linter
  )
})
