context("no_tab_linter")

test_that("returns the correct linting", {
  msg <- rex("Use spaces to indent, not tabs.")
  expect_lint("blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("#\tblah", NULL, no_tab_linter)

  expect_lint("\tblah", msg, no_tab_linter)

  expect_lint("\t\tblah", msg, no_tab_linter)
})
