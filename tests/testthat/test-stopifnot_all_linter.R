test_that("stopifnot_all_linter skips allowed usages", {
  expect_lint("stopifnot(all(x) || any(y))", NULL, stopifnot_all_linter())
})

test_that("stopifnot_all_linter blocks simple disallowed usages", {
  linter <- stopifnot_all_linter()
  lint_msg <- rex::rex("stopifnot(x) runs all() 'under the hood'")

  expect_lint("stopifnot(all(A))", list(lint_msg, column_number = 11L), linter)
  expect_lint("stopifnot(x, y, all(z))", list(lint_msg, column_number = 17L), linter)

  expect_lint(
    trim_some("{
      stopifnot(all(x), all(y),
        all(z)
      )
      stopifnot(a > 0, b < 0, all(c == 0))
    }"),
    list(
      list(lint_msg, line_number = 2L, column_number = 13L),
      list(lint_msg, line_number = 2L, column_number = 21L),
      list(lint_msg, line_number = 3L, column_number = 5L),
      list(lint_msg, line_number = 5L, column_number = 27L)
    ),
    linter
  )
})
