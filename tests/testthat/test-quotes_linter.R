test_that("quotes_linter skips allowed usages", {
  linter <- quotes_linter()

  expect_lint("blah", NULL, linter)
  expect_lint('"blah"', NULL, linter)
  expect_lint('"\'blah"', NULL, linter)
  expect_lint('"blah\'"', NULL, linter)
  expect_lint('"\'blah\'"', NULL, linter)
  expect_lint("'\"'", NULL, linter)
  expect_lint("'\"blah\"'", NULL, linter)
})

test_that("quotes_linter blocks disallowed usages", {
  linter <- quotes_linter()
  lint_msg <- rex::rex("Only use double-quotes.")

  expect_lint("'blah'", lint_msg, linter)
  expect_lint("fun('blah')", lint_msg, linter)
  expect_lint("{'blah'}", lint_msg, linter)

  expect_lint(
    "
    x = 'test
    '",
    list(message = lint_msg, ranges = list(c(9L, 13L))),
    linter
  )
})

# NB: repeat of above tests with (mostly) opposite results
test_that("quotes_linter skips allowed usages of delimiter='", {
  linter <- quotes_linter(delimiter = "'")

  expect_lint("blah", NULL, linter)
  expect_lint('"\'blah"', NULL, linter)
  expect_lint('"blah\'"', NULL, linter)
  expect_lint('"\'blah\'"', NULL, linter)
  expect_lint("'\"'", NULL, linter)
  expect_lint("'\"blah\"'", NULL, linter)
  expect_lint("'blah'", NULL, linter)
  expect_lint("fun('blah')", NULL, linter)
  expect_lint("{'blah'}", NULL, linter)
})

test_that("quotes_linter blocks disallowed usages of delimiter='", {
  linter <- quotes_linter(delimiter = "'")
  lint_msg <- rex::rex("Only use single-quotes.")

  expect_lint('"blah"', lint_msg, linter)
  expect_lint(
    '
    x = "test
    "',
    list(message = lint_msg, ranges = list(c(9L, 13L))),
    linter
  )
})

test_that("handles R>=4.0.0 strings", {
  skip_if_not_r_version("4.0.0")
  linter <- quotes_linter()
  expect_lint('R"(hello \'\' there)"', NULL, linter)
  expect_lint("R'( whoops )'", rex::rex("Only use double-quotes."), linter)
  expect_lint("R'---[ daisy ]---'", rex::rex("Only use double-quotes."), linter)
  expect_lint("r'(\")'", NULL, linter)
})

test_that("single_quotes_linter is defunct", {
  expect_error(single_quotes_linter(), "Use quotes_linter instead", fixed = TRUE)
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Only use double-quotes.")

  expect_lint(
    trim_some("{
      'abc'
      'def'
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    quotes_linter()
  )
})
