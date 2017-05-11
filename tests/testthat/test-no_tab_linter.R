context("no_tab_linter")

test_that("returns the correct linting", {
  msg <- rex("Use spaces to indent, not tabs.")
  expect_lint("blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("#\tblah", NULL, no_tab_linter)

  expect_lint(
    "\tblah",
    list(c(message = msg, line_number = 1L, column_number = 1L)),
    no_tab_linter
  )

  expect_lint(
    "\n\t\t\tblah",
    list(c(message = msg, line_number = 2L, column_number = 1L)),
    no_tab_linter
  )
})
