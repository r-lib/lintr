context("no_tab_linter")

test_that("returns the correct linting", {
  msg <- rex("Use spaces to indent, not tabs.")
  expect_lint("blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("#\tblah", NULL, no_tab_linter)

  expect_lint(
    "\tblah",
    list(message = msg, line_number = 1L, column_number = 1, ranges = list(c(1L, 1L))),
    no_tab_linter)

  expect_lint("\n\t\tblah", c(message = msg, line_number = 2L), no_tab_linter)

  # Note: no tests of column number since they are currently incorrect (tabs converted to 8 spaces)
})
