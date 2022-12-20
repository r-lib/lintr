test_that("unnecessary_nested_if_linter skips allowed usages", {
  linter <- unnecessary_nested_if_linter()

  expect_lint(
    trim_some("
      if (x && y) {
        1L
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
    for (x in 1:3) {
      if (x && y) {
        1L
      }
    }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        1L
      } else if (y) {
        2L
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        1L
      } else {
        if (y) {
          2L
        }
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (if (x) TRUE else FALSE) {
        1L
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        y <- x + 1L
        if (y) {
          1L
        }
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if ((x && y) || (if (x) TRUE else FALSE)) {
        1L
      }"),
    NULL,
    linter
  )

  # if there is any additional code between the inner and outer scopes, no lint
  expect_lint(
    trim_some("
      if (x && a) {
        y <- x + 1L
        if (y || b) {
          1L
        }
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        if (y) {
          1L
        }
        y <- x + 1L
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        y <- x + 1L
        if (y) {
          1L
        }
        y <- x
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        y <- x + 1L
        {
          if (y) {
            1L
          }
        }
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        {
           y <- x + 1L
           if (y) {
             1L
           }
        }
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        {
          if (y) {
            1L
          }
        }
        y <- x + 1L
      }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        {
          y <- x + 1L
          {
            if (y) {
              1L
            }
          }
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
          1L
        }
      }"),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        if (y) 1L
      }"),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      if (x && a) {
        if (y || b) {
          1L
        }
      }"),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      if (if (x) TRUE else FALSE) {
        if (y) {
          1L
        }
      }"),
    lint_message,
    linter
  )

  expect_lint(
    "if (x) if (y) 1L",
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
    for (x in 1:3) {
      if (x) if (y) 1L
    }"),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        if (y) {
          if (z) {
            1L
          }
        }
      }"),
    list(
      list(message = lint_message, line_number = 2L, column_number = 3L),
      list(message = lint_message, line_number = 3L, column_number = 5L)
    ),
    linter
  )
})
