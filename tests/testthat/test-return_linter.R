test_that("Lint return on end of function", {
  expect_lint(
    trim_some("
      function() {
        return(1)
        # Test
        3 + 4
      }
    "),
    list(
      line_number = 4L,
      message = rex::rex("An explicit return at the end of a function is desired")
    ),
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    trim_some("
      function() {
        return(1)
      }
    "),
    list(
      line_number = 2L,
      message = rex::rex("An explicit return at the end of a function is not required")
    ),
    return_linter()
  )
})

test_that("Lint return on end of lambda function", {
  skip_if_not_r_version("4.1.0")

  expect_lint(
    trim_some("
      \\(bar) {
        5L + 3L
      }
    "),
    list(
      line_number = 2L,
      message = rex::rex("An explicit return at the end of a function is desired")
    ),
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    trim_some("
      \\(bar) {
        5L + 3L
        return(1)
      }
    "),
    list(
      line_number = 3L,
      message = rex::rex("An explicit return at the end of a function is not required")
    ),
    return_linter()
  )
})

