test_that("single_quotes_linter skips allowed usages", {
  linter <- single_quotes_linter()

  expect_lint("blah", NULL, linter)
  expect_lint("\"blah\"", NULL, linter)
  expect_lint("\"'blah\"", NULL, linter)
  expect_lint("\"blah'\"", NULL, linter)
  expect_lint("\"blah'\"", NULL, linter)
  expect_lint("\"'blah'\"", NULL, linter)
  expect_lint("'\"'", NULL, linter)
  expect_lint("'\"blah\"'", NULL, linter)
})

test_that("single_quotes_linter blocks disallowed usages", {
  linter <- single_quotes_linter()
  lint_msg <- rex::rex("Only use double-quotes.")

  expect_lint("'blah'", lint_msg, linter)
  expect_lint("fun('blah')", lint_msg, linter)
  expect_lint("{'blah'}", lint_msg, linter)

  expect_lint(
    "
    x = 'test
    '",
    list(
      message = "Only use double-quotes.",
      ranges = list(c(9L, 13L))
    ),
    linter
  )
})

test_that("handles R>=4.0.0 strings", {
  skip_if_not_r_version("4.0.0")
  expect_lint('R"(hello \'\' there)"', NULL, single_quotes_linter())
  expect_lint("R'( whoops )'", rex::rex("Only use double-quotes."), single_quotes_linter())
  expect_lint("R'---[ daisy ]---'", rex::rex("Only use double-quotes."), single_quotes_linter())
  expect_lint("r'(\")'", NULL, single_quotes_linter())
})
