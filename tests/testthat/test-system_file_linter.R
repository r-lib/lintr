test_that("system_file_linter skips allowed usages", {
  expect_lint("system.file('a', 'b', 'c')", NULL, system_file_linter())
  expect_lint("file.path('a', 'b', 'c')", NULL, system_file_linter())
})

test_that("system_file_linter blocks simple disallowed usages", {
  expect_lint(
    "system.file(file.path('path', 'to', 'data'), package = 'foo')",
    rex::rex("Use the `...` argument of system.file() to expand paths"),
    system_file_linter()
  )

  expect_lint(
    "file.path(system.file(package = 'foo'), 'path', 'to', 'data')",
    rex::rex("Use the `...` argument of system.file() to expand paths"),
    system_file_linter()
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    linter
  )
})
