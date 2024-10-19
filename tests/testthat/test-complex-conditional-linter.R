test_that("complex_conditional_linter skips simple conditionals", {
  linter <- complex_conditional_linter()

  expect_lint(
    trim_some("
      if (x > 0) {
        print('simple')
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x > 0 && y < 10) {
        print('two conditions')
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      while (i <= n && !done) {
        i <- i + 1
      }
    "),
    NULL,
    linter
  )
})

test_that("complex_conditional_linter finds complex conditions", {
  linter <- complex_conditional_linter(threshold = 2L)
  lint_message <- rex::rex("Complex conditional with more than 2 logical operands")

  expect_lint(
    trim_some("
      if (x > 0 && y < 10 && z == TRUE) {
        print('complex')
      }
    "),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      while (a > b || c < d && e == f) {
        next
      }
    "),
    lint_message,
    linter
  )
})

test_that("complex_conditional_linter handles nested conditionals", {
  linter <- complex_conditional_linter(threshold = 2L)
  lint_message <- rex::rex("Complex conditional with more than 2 logical operands")

  # Simple outer, complex inner
  expect_lint(
    trim_some("
      if (x > 0) {
        if (a == 1 && b == 2 && c == 3) {
          print('nested')
        }
      }
    "),
    lint_message,
    linter
  )

  # Multiple complex conditions
  expect_lint(
    trim_some("
      if (x > 0 && y < 10 && z == TRUE) {
        while (a && b && c) {
          print('double complex')
        }
      }
    "),
    list(
      list(message = lint_message, line_number = 2L),
      list(message = lint_message, line_number = 3L)
    ),
    linter
  )
})

test_that("complex_conditional_linter respects threshold parameter", {
  expect_lint(
    trim_some("
      if (a && b && c) {
        print('test')
      }
    "),
    NULL,
    complex_conditional_linter(threshold = 3L)
  )

  expect_lint(
    trim_some("
      if (a && b && c && d) {
        print('test')
      }
    "),
    rex::rex("Complex conditional with more than 3 logical operands"),
    complex_conditional_linter(threshold = 3L)
  )
})

test_that("complex_conditional_linter handles mixed operators and parentheses", {
  linter <- complex_conditional_linter(threshold = 2L)
  lint_message <- rex::rex("Complex conditional with more than 2 logical operands")

  expect_lint(
    trim_some("
      if ((a && b) || (c && d) || e) {
        print('mixed')
      }
    "),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      if (a && (b || c) && d) {
        print('nested ops')
      }
    "),
    lint_message,
    linter
  )
})

test_that("complex_conditional_linter skips non-conditional expressions", {
  linter <- complex_conditional_linter(threshold = 2L)

  expect_lint(
    trim_some("
      x <- a && b && c && d
      if (x) {
        print('okay')
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      result <- all(
        a > 0,
        b < 10,
        c != 0,
        !is.na(d)
      )
      if (result) {
        print('clean')
      }
    "),
    NULL,
    linter
  )
})
