test_that("unnecessary_nested_if_linter skips allowed usages", {
  linter <- unnecessary_nested_if_linter()

  expect_lint(
    trim_some("
      if (x && y) {
        return(1L)
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        return(1L)
      } else if (y) {
        return(2L)
      }"),
    NULL,
    linter
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
    linter
  )

  expect_lint(
    trim_some("
      if (if (x) TRUE else FALSE) {
        return(1L)
      }"),
    NULL,
    linter
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
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        if (y) {
          return(1L)
        }
        y <- x + 1L
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if ((x && y) || (if (x) TRUE else FALSE)) {
        return(1L)
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x && a) {
        y <- x + 1L
        if (y || b) {
          return(1L)
        }
      }"),
    NULL,
    linter
  )
})

test_that("unnecessary_nested_if_linter blocks disallowed usages", {
  lint_message <- rex::rex("Don't use nested `if` statements")
  linter <- unnecessary_nested_if_linter()

  expect_lint(
    trim_some("
      if (x) {
        if (y) {
          return(1L)
        }
      }"),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      if (x && a) {
        if (y || b) {
          return(1L)
        }
      }"),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      if (if (x) TRUE else FALSE) {
        if (y) {
          return(1L)
        }
      }"),
    lint_message,
    linter
  )
})
