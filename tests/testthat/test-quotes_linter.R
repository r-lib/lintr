test_that("quotes_linter skips allowed usages", {
  linter <- quotes_linter()

  expect_no_lint("blah", linter)
  expect_no_lint('"blah"', linter)
  expect_no_lint('"\'blah"', linter)
  expect_no_lint('"blah\'"', linter)
  expect_no_lint('"\'blah\'"', linter)
  expect_no_lint("'\"'", linter)
  expect_no_lint("'\"blah\"'", linter)
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

  expect_no_lint("blah", linter)
  expect_no_lint('"\'blah"', linter)
  expect_no_lint('"blah\'"', linter)
  expect_no_lint('"\'blah\'"', linter)
  expect_no_lint("'\"'", linter)
  expect_no_lint("'\"blah\"'", linter)
  expect_no_lint("'blah'", linter)
  expect_no_lint("fun('blah')", linter)
  expect_no_lint("{'blah'}", linter)
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

test_that("raw strings are handled correctly", {
  linter <- quotes_linter()
  expect_no_lint('R"(hello \'\' there)"', linter)
  expect_lint("R'( whoops )'", rex::rex("Only use double-quotes."), linter)
  expect_lint("R'---[ daisy ]---'", rex::rex("Only use double-quotes."), linter)
  expect_no_lint("r'(\")'", linter)
})

test_that("single_quotes_linter is deprecated", {
  expect_warning(
    {
      old_linter <- single_quotes_linter()
    },
    "Use quotes_linter instead",
    fixed = TRUE
  )
  expect_no_lint('"blah"', old_linter)
  expect_lint("'blah'", "Only use double-quotes", old_linter)
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
