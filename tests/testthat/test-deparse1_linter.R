test_that("deparse1_linter skips allowed usages", {
  linter <- deparse1_linter()

  expect_no_lint("deparse1(x)", linter)
  expect_no_lint("deparse(x)", linter)
  # no collapse
  expect_no_lint("paste(deparse(x))", linter)
  # multiple positional args to paste
  expect_no_lint('paste("Error: ", deparse(x), collapse = " ")', linter)
  expect_no_lint('paste(x, collapse = " ")', linter)
})

test_that("deparse1_linter blocks simple disallowed usages", {
  linter <- deparse1_linter()
  lint_msg <- rex::rex("Use deparse1(x) instead of paste(deparse(x), collapse = ...).")

  expect_lint('paste(deparse(x), collapse = " ")', lint_msg, linter)
  expect_lint('paste0(deparse(x), collapse = "")', lint_msg, linter)
  expect_lint('paste(deparse(x, width.cutoff = 500L), collapse = " ")', lint_msg, linter)
  # arg order doesn't matter
  expect_lint('paste(collapse = " ", deparse(x))', lint_msg, linter)
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Use deparse1(x) instead of paste(deparse(x), collapse = ...).")

  expect_lint(
    trim_some("{
      paste(deparse(x), collapse = ' ')
      paste(deparse(y), collapse = ' ')
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    deparse1_linter()
  )
})
