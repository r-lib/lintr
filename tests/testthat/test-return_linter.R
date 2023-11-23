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
      message = rex::rex("All functions must have an explicit return().")
    ),
    return_linter(return_style = "explicit")
  )

  expect_lint(
    trim_some("
      function() {
        return(1)
      }
    "),
    list(
      line_number = 2L,
      message = rex::rex("Use implicit return behavior; explicit return() is not needed.")
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
      message = rex::rex("All functions must have an explicit return().")
    ),
    return_linter(return_style = "explicit")
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
      message = rex::rex("Use implicit return behavior; explicit return() is not needed.")
    ),
    return_linter()
  )
})

test_that("Do not lint if/else statements (with return) on end of function", {
  linter <- return_linter(return_style = "explicit")

  expect_lint(
    trim_some("
      function() {
        if (x) {
          return(4)
        } else if (y) {
          return(5)
        } else {
          return(6)
        }
      }
    "),
    NULL,
    linter
  )
})

test_that("Lint control statements (without return) on end of function", {
  linter <- return_linter(return_style = "explicit")
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      function() {
        while (x > 4) {
          cat(4)
          if (x < 4) {
            return(x)
          }
        }
      }
    "),
    list(lint_msg, line_number = 2L),
    linter
  )

  expect_lint(
    trim_some("
      function() {
        repeat {
          cat(4)
        }
      }
    "),
    list(lint_msg, line_number = 2L),
    linter
  )

  expect_lint(
    trim_some("
      function() {
        for (i in 1:10) {
          cat(4)
          if (i > 11) {
            return(x)
          }
        }
      }
    "),
    list(lint_msg, line_number = 2L),
    linter
  )

  expect_lint(
    trim_some("
      function() {
        if (x == 2L){
          return(e)
        } else if (x == 3L) {
          cat(f)
        }
      }
    "),
    list(lint_msg, line_number = 4L),
    linter
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
    return_linter(return_style = "explicit")
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

test_that("Do not lint stop on end of function", {
  linter <- return_linter(return_style = "explicit")
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      function(x) {
        switch(x, a = 1, 'b' = 2, '3' = 3, 4)
      }
    "),
    list(lint_msg, line_number = 2L),
    linter
  )

  expect_lint(
    trim_some("
      function(x) {
        switch(x, a = return(1), 'b' = stop(2), '3' = return(3), 4)
      }
    "),
    list(lint_msg, line_number = 2L),
    linter
  )

  expect_lint(
    trim_some("
      function() {
        switch(
          x,
          a = return(1),
          'b' = stop(2),
          '3' = return(3)
        )
      }
    "),
    list(lint_msg, line_number = 2L),
    linter
  )

  expect_lint(
    trim_some("
      function(x) {
        switch(x, a = return(1), 'b' = stop(2), '3' = return(3), stop('End'))
      }
    "),
    list(lint_msg, line_number = 2L),
    linter
  )
})

test_that("return_linter works in simple function", {
  expect_lint(
    trim_some("
      foo <- function(bar) {
        return(bar)
      }
    "),
    NULL,
    return_linter(return_style = "explicit")
  )
})

test_that("return_linter works for using stop() instead of returning", {
  linter <- return_linter(return_style = "explicit")

  expect_lint(
    trim_some("
      foo <- function(bar) {
        stop('bad')
      }
    "),
    NULL,
    linter
  )
})

test_that("return_linter ignores expressions that aren't functions", {
  expect_lint("x + 1", NULL, return_linter(return_style = "explicit"))
})

test_that("return_linter ignores anonymous/inline functions", {
  expect_lint("lapply(rnorm(10), function(x) x + 1)", NULL, return_linter(return_style = "explicit"))
})

test_that("return_linter ignores if statements outside of functions", {
  expect_lint(
    trim_some("
      if (TRUE) {
        TRUE
      } else {
        FALSE
      }
    "),
    NULL,
    return_linter(return_style = "explicit")
  )
})

test_that("return_linter passes on multi-line functions", {
  expect_lint(
    trim_some("
      foo <- function(x) {
        y <- x + 1
        return(y)
      }
    "),
    NULL,
    return_linter(return_style = "explicit")
  )
})


test_that("return_linter identifies a simple missing return", {
  expect_lint(
    trim_some("
      foo <- function(bar) {
        bar
      }
    "),
    rex::rex("All functions must have an explicit return()."),
    return_linter(return_style = "explicit")
  )
})


test_that("return_linter finds a missing return in a 2+ line function", {
  expect_lint(
    trim_some("
      foo <- function(x) {
        y <- x + 1
        y^2
      }
    "),
    rex::rex("All functions must have an explicit return()."),
    return_linter(return_style = "explicit")
  )
})

test_that("return_linter finds a missing return despite early returns", {
  expect_lint(
    trim_some("
      foo <- function(x) {
        if (TRUE) return(TRUE)
        x <- 1 + 1
        x
      }
    "),
    rex::rex("All functions must have an explicit return()."),
    return_linter(return_style = "explicit")
  )
})


test_that("return_linter finds multiple missing returns in branches", {
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          TRUE
        } else {
          FALSE
        }
      }
    "),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 4L)
    ),
    return_linter(return_style = "explicit")
  )
})

test_that("return_linter works regardless of braces in final if case", {
  linter <- return_linter(return_style = "explicit")

  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) TRUE
      }
    "),
    rex::rex("All functions must have an explicit return()."),
    linter
  )
  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) return(TRUE)
      }
    "),
    NULL,
    linter
  )
})

test_that("return_linter finds missing return in one branch of an if", {
  linter <- return_linter(return_style = "explicit")
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          return(TRUE)
        } else {
          FALSE
        }
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          TRUE
        } else {
          return(FALSE)
        }
      }
    "),
    lint_msg,
    linter
  )
})

test_that("return_linter works in nested if statements", {
  linter <- return_linter(return_style = "explicit")

  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          return(TRUE)
        } else if (nzchar('a')) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          if (nzchar('a')) {
            TRUE
          }
        } else {
          return(FALSE)
        }
      }
    "),
    rex::rex("All functions must have an explicit return()."),
    linter
  )
})

test_that("return_linter works in multi-line nested if statements", {
  linter <- return_linter(return_style = "explicit")

  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          if (nzchar('a')) {
            y <- 1 + 1
            y
          }
        } else {
          return(FALSE)
        }
      }
    "),
    rex::rex("All functions must have an explicit return()."),
    linter
  )

  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          if (nzchar('a')) {
            y <- 1 + 1
            return(y)
          }
        } else {
          return(FALSE)
        }
      }
    "),
    NULL,
    linter
  )
})

test_that("return_linter works for final for loops as well", {
  linter <- return_linter(return_style = "explicit")
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      foo <- function() {
        for (i in seq_len(10)) {
          if (i %% 2 == 0) {
            y <- 1 + 1
            return(y)
          }
        }
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function() {
        for (i in seq_len(10)) {
          if (i %% 2 == 0) {
            y <- 1 + 1
          }
        }
      }
    "),
    lint_msg,
    linter
  )
})

test_that("return_linter works for function factories", {
  linter <- return_linter(return_style = "explicit")
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      foo <- function(x) {
        function () {
          return(x + 1)
        }
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        function () {
          x + 1
        }
      }
    "),
    list(lint_msg, lint_msg),
    linter
  )
})

test_that("return_linter allows return()-less Rcpp wrappers", {
  expect_lint(
    trim_some("
      ReadCapacitorAsList <- function(file) {
        .Call(R_ReadCapacitorAsList, file)
      }
    "),
    NULL,
    return_linter(return_style = "explicit")
  )
})

test_that("return_linter allows return()-less namespace hook calls", {
  expect_lint(
    trim_some("
      .onLoad <- function(libname, pkgname) {
        do_setup()
      }
    "),
    NULL,
    return_linter(return_style = "explicit")
  )
})

test_that("return_linter correctly handles pipes", {
  linter <- return_linter(return_style = "explicit")

  expect_lint(
    trim_some("
      foo <- function(x) {
        x %>%
          return()
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        x %>%
          mean() %>%
          return()
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        y <- rnorm(length(x))

        x %>%
          cbind(y) %>%
          return()
      }
    "),
    NULL,
    linter
  )
})

test_that("return_linter handles pipes in control flow", {
  linter <- return_linter(return_style = "explicit")
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      foo <- function(x) {
        if (TRUE) {
          return(invisible())
        } else {
          x %>%
            return()
        }
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        for (i in seq_len(10)) {
          x %>%
            mean()
        }
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        if (TRUE) {
          x %>%
            mean()
        } else {
          return(TRUE)
        }
      }
    "),
    lint_msg,
    linter
  )
})

test_that("return_linter passes on q() or quit() calls", {
  expect_lint(
    trim_some("
      foo <- function(x) {
        if (TRUE) {
          q('n')
        } else {
          quit('n')
        }
      }
    "),
    NULL,
    return_linter(return_style = "explicit")
  )
})

test_that("return_functions= argument works", {
  linter <- return_linter(return_style = "explicit", return_functions = "LOG")

  expect_lint(
    trim_some("
      foo <- function(bar) {
        LOG('INFO', 'bad')
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(bar) {
        logging::LOG('INFO', 'bad')
      }
    "),
    NULL,
    linter
  )
})

test_that("except= argument works", {
  expect_lint(
    trim_some("
      foo <- function(bar) {
        5 + 3
      }
    "),
    NULL,
    return_linter(return_style = "explicit", except = "foo")
  )
})

test_that("return_linter skips brace-wrapped inline functions", {
  expect_lint("function(x) { sum(x) }", NULL, return_linter(return_style = "explicit"))
})

test_that("return_linter skips common S4 method functions", {
  linter <- return_linter(return_style = "explicit")

  expect_lint(
    trim_some("
      setGeneric(
        'ReadCircuitsPBAsDataTable',
        function(pbMessageList) {
          standardGeneric('ReadCircuitsPBAsDataTable')
        }
      )
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      setMethod('initialize', 'CircuitsTopology', function(.Object, ...) {
        callNextMethod(.Object, ...)
      })
    "),
    NULL,
    linter
  )
})

test_that("return_functions= is not affected by namespace qualification", {
  linter <- return_linter(return_style = "explicit", return_functions = "abort")

  expect_lint(
    trim_some("
      foo <- function(bar) {
        abort('bad')
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(bar) {
        rlang::abort('bad')
      }
    "),
    NULL,
    linter
  )
})

test_that("return_linter skips invokeRestart(), tryInvokeRestart()", {
  linter <- return_linter(return_style = "explicit")

  expect_lint(
    trim_some("
      warning = function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart('muffleWarning')
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      custom_warning = function(w) {
        warn <<- append(warn, conditionMessage(w))
        tryInvokeRestart('muffleCustom_warning')
      }
    "),
    NULL,
    linter
  )
})

# NB: x |> return() is blocked by the parser, so no need to test that.
test_that("Native pipes are handled correctly", {
  skip_if_not_r_version("4.1.0")

  linter <- return_linter(return_style = "explicit")
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      foo <- function(x) {
        for (i in seq_len(10)) {
          x |>
            mean()
        }
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        if (TRUE) {
          x |>
            mean()
        } else {
          return(TRUE)
        }
      }
    "),
    lint_msg,
    linter
  )
})

test_that("return_linter works for final while/repeat loops as well", {
  linter <- return_linter(return_style = "explicit")
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      foo <- function(x) {
        while (x > 0) {
          if (x %% 2 == 0) {
            return(x)
          }
          x <- x + sample(10, 1)
        }
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        repeat {
          if (x == 0) {
            return(x)
          }
          x <- x - sign(x)
        }
      }
    "),
    lint_msg,
    linter
  )
})

test_that("return_linter lints `message`, `warning` and `stopifnot`", {
  linter <- return_linter(return_style = "explicit")
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      foo <- function(bar) {
        stopifnot(bar == 'd')
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(bar) {
        message('test')
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(bar) {
        warning(test)
      }
    "),
    lint_msg,
    linter
  )
})
