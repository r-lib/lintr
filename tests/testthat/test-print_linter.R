test_that("print_linter skips allowed usages", {
  linter <- print_linter()

  expect_no_lint("print(x)", linter)
  expect_no_lint("print(foo(x))", linter)
})

test_that("print_linter blocks disallowed usages", {
  linter <- print_linter()
  lint_msg <-
    rex::rex("Use cat() instead of print() logging messages. Use message() in cases calling for a signalled condition.")

  expect_lint('print("hi")', lint_msg, linter)

  # basic known-character functions
  expect_lint('print(paste(x, "b", y))', lint_msg, linter)
  expect_lint('print(paste0(x, "c", y))', lint_msg, linter)
  expect_lint('print(sprintf("a %s", x))', lint_msg, linter)

  # vectorization, metadata
  expect_lint(
    trim_some("{
      print('a')
      print(paste('x', y))
      print(z)
      print(sprintf('%s', b))
    }"),
    list(
      list(lint_msg, line_number = 2L, column_number = 3L),
      list(lint_msg, line_number = 3L, column_number = 3L),
      list(lint_msg, line_number = 5L, column_number = 3L)
    ),
    linter
  )
})
