test_that("cbind_dataframe_linter skips allowed usages", {
  linter <- cbind_dataframe_linter()

  expect_no_lint("cbind.data.frame(x, x)", linter)
  expect_no_lint("do.call(mean, x)", linter)

  # Other cbind methods
  expect_no_lint("do.call(cbind, x)", linter)
})

test_that("cbind_dataframe_linter blocks simple disallowed usages", {
  linter <- cbind_dataframe_linter()
  lint_message <- rex::rex("use `data.frame(lst)`")

  expect_lint("do.call(cbind.data.frame, x)", lint_message, linter)
})

test_that("lints vectorize", {
  lint_message <- rex::rex("use `data.frame(lst)`")

  expect_lint(
    trim_some("{
      cbind(a, b)
      do.call(cbind.data.frame, x)
      do.call(cbind.data.frame, y)
    }"),
    list(
      list(lint_message, line_number = 3L),
      list(lint_message, line_number = 4L)
    ),
    cbind_dataframe_linter()
  )
})
