context("no_tab_linter")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("#\tblah", NULL, no_tab_linter)

  expect_lint("\tblah",
    rex("Use two spaces to indent, never tabs."),
      no_tab_linter)

  expect_lint("\t\tblah",
    rex("Use two spaces to indent, never tabs."),
      no_tab_linter)

})
