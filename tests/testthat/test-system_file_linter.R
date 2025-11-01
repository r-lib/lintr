test_that("system_file_linter skips allowed usages", {
  linter <- system_file_linter()

  expect_no_lint("system.file('a', 'b', 'c')", linter)
  expect_no_lint("file.path('a', 'b', 'c')", linter)
})

test_that("system_file_linter blocks simple disallowed usages", {
  linter <- system_file_linter()
  lint_msg <- rex::rex("Use the `...` argument of system.file() to expand paths")

  expect_lint("system.file(file.path('path', 'to', 'data'), package = 'foo')", lint_msg, linter)
  expect_lint("file.path(system.file(package = 'foo'), 'path', 'to', 'data')", lint_msg, linter)
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Use the `...` argument of system.file() to expand paths")

  expect_lint(
    trim_some("{
      file.path(system.file(package = 'foo'), 'bar')
      system.file(file.path('bar', 'data'), package = 'foo')
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    system_file_linter()
  )
})
