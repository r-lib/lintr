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

test_that("Do not lint controll statments (with return) on end of function", {
  expect_lint(
    trim_some("
      function() {
        repeat {
          cat(4)
        }
      }
    "),
    NULL,
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    trim_some("
      function() {
        if(x) {
          return(4)
        } else if(y) {
          return(5)
        } else {
          return(6)
        }
      }
    "),
    NULL,
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("Lint controll statments (without return) on end of function", {
  msg <- rex::rex("An explicit return at the end of a function is desired")

  expect_lint(
    trim_some("
      function() {
        while(x > 4) {
          cat(4)
          if(x < 4) {
            return(x)
          }
        }
      }
    "),
    msg,
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    trim_some("
      function() {
        for(i in 1:10) {
          cat(4)
          if(i > 11) {
            return(x)
          }
        }
      }
    "),
    msg,
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    trim_some("
      function() {
        if(x == 2L){
          return(e)
        }
      }
    "),
    msg,
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    trim_some("
      function() {
        if(x == 2L){
          return(e)
        } else if(x == 3L) {
          cat(f)
        }
      }
    "),
    msg,
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("Do not lint stop on end of function", {
  expect_lint(
    trim_some("
      function() {
        # Test
        3 + 4
        stop(1)
      }
    "),
    NULL,
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    trim_some("
      function() {
        stop(1)
      }
    "),
    NULL,
    return_linter()
  )
})