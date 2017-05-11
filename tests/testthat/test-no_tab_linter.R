context("no_tab_linter")

test_that("returns the correct linting", {
  msg <- rex("Use spaces to indent, not tabs.")
  linter <- no_tab_linter()
  expect_is(linter, "linter")

  expect_lint("blah", NULL, linter)

  expect_lint("  blah", NULL, linter)

  expect_lint("  blah", NULL, linter)

  expect_lint("#\tblah", NULL, linter)

  expect_lint("\tblah", c(message = msg, line_number = 1L), linter)

  expect_lint("\n\t\tblah", c(message = msg, line_number = 2L), linter)

  # Note: no tests of column number since they are currently incorrect (tabs converted to 8 spaces)
})
