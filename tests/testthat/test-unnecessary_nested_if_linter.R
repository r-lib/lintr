test_that("unnecessary_nested_if_linter skips allowed usages", {
  linter <- unnecessary_nested_if_linter()

  expect_lint("if (x > 0) TRUE", NULL, linter)
  expect_lint("if (x > 5) TRUE else NA", NULL, linter)

  expect_lint(
    trim_some("
      if (x && y) {
        return(1L)
      }"),
    NULL,
    unnecessary_nested_if_linter()
  )

  expect_lint(
    trim_some("
      if (x) {
        return(1L)
      } else if (y) {
        return(2L)
      }"),
    NULL,
    unnecessary_nested_if_linter()
  )

  expect_lint(
    trim_some("
      if (x) {
        return(1L)
      } else {
        if (y) {
          return(2L)
        }
      }"),
    NULL,
    unnecessary_nested_if_linter()
  )

  expect_lint(
    trim_some("
      if (x) {
        y <- x + 1L
        if (y) {
          return(1L)
        }
      }"),
    NULL,
    unnecessary_nested_if_linter()
  )
})

test_that("unnecessary_nested_if_linter blocks simple disallowed usages", {
  lint_message <- rex::rex("Don't use nested `if()` statements")

  expect_lint(
    trim_some("
      if (x) {
        if (y) {
          return(1L)
        }
      }"),
    lint_message,
    unnecessary_nested_if_linter()
  )
})
