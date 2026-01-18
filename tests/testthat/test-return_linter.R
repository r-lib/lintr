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
      message = rex::rex("All functions must have an explicit return()."),
      type = "warning"
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
      message = rex::rex("Use implicit return behavior; explicit return() is not needed."),
      type = "style"
    ),
    return_linter()
  )
})

test_that("Lint return on end of lambda function", {
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

  expect_no_lint(
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
    list(lint_msg, line_number = 5L),
    linter
  )
})

test_that("Do not lint stop on end of function", {
  expect_no_lint(
    trim_some("
      function() {
        # Test
        3 + 4
        stop(1)
      }
    "),
    return_linter(return_style = "explicit")
  )

  expect_no_lint(
    trim_some("
      function() {
        stop(1)
      }
    "),
    return_linter()
  )
})

test_that("return_linter works in simple function", {
  expect_no_lint(
    trim_some("
      foo <- function(bar) {
        return(bar)
      }
    "),
    return_linter(return_style = "explicit")
  )
})

test_that("return_linter works for using stop() instead of returning", {
  linter <- return_linter(return_style = "explicit")

  expect_no_lint(
    trim_some("
      foo <- function(bar) {
        stop('bad')
      }
    "),
    linter
  )
})

test_that("return_linter ignores expressions that aren't functions", {
  expect_no_lint("x + 1", return_linter(return_style = "explicit"))
})

test_that("return_linter ignores anonymous/inline functions", {
  expect_no_lint("lapply(rnorm(10), function(x) x + 1)", return_linter(return_style = "explicit"))
})

test_that("return_linter ignores if statements outside of functions", {
  expect_no_lint(
    trim_some("
      if (TRUE) {
        TRUE
      } else {
        FALSE
      }
    "),
    return_linter(return_style = "explicit")
  )
})

test_that("return_linter passes on multi-line functions", {
  expect_no_lint(
    trim_some("
      foo <- function(x) {
        y <- x + 1
        return(y)
      }
    "),
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
      list(lint_msg, line_number = 3L),
      list(lint_msg, line_number = 5L)
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
  expect_no_lint(
    trim_some("
      foo <- function() {
        if (TRUE) return(TRUE)
      }
    "),
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

  expect_no_lint(
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

  expect_no_lint(
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
  expect_no_lint(
    trim_some("
      ReadCapacitorAsList <- function(file) {
        .Call(R_ReadCapacitorAsList, file)
      }
    "),
    return_linter(return_style = "explicit")
  )
})

test_that("return_linter allows return()-less namespace hook calls", {
  expect_no_lint(
    trim_some("
      .onLoad <- function(libname, pkgname) {
        do_setup()
      }
    "),
    return_linter(return_style = "explicit")
  )
})

test_that("return_linter correctly handles pipes", {
  linter <- return_linter(return_style = "explicit")

  expect_no_lint(
    trim_some("
      foo <- function(x) {
        x %>%
          return()
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      foo <- function(x) {
        x %>%
          mean() %>%
          return()
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      foo <- function(x) {
        y <- rnorm(length(x))

        x %>%
          cbind(y) %>%
          return()
      }
    "),
    linter
  )
})

test_that("return_linter handles pipes in control flow", {
  linter <- return_linter(return_style = "explicit")
  lint_msg <- rex::rex("All functions must have an explicit return().")

  expect_no_lint(
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
  expect_no_lint(
    trim_some("
      foo <- function(x) {
        if (TRUE) {
          q('n')
        } else {
          quit('n')
        }
      }
    "),
    return_linter(return_style = "explicit")
  )
})

test_that("return_functions= argument works", {
  linter <- return_linter(return_style = "explicit", return_functions = "LOG")

  expect_no_lint(
    trim_some("
      foo <- function(bar) {
        LOG('INFO', 'bad')
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      foo <- function(bar) {
        logging::LOG('INFO', 'bad')
      }
    "),
    linter
  )
})

test_that("except= argument works", {
  expect_no_lint(
    trim_some("
      foo <- function(bar) {
        5 + 3
      }
    "),
    return_linter(return_style = "explicit", except = "foo")
  )
})

test_that("except_regex= argument works", {
  linter <- return_linter(return_style = "explicit", except_regex = "^Test")

  expect_no_lint(
    trim_some("
      TestSummary <- function() {
        context <- foo(72643424)
        expected <- data.frame(a = 2)
        checkEquals(expected, bar(context))
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      TestMyPackage <- function() {
        checkMyCustomComparator(x, y)
      }
    "),
    linter
  )

  expect_lint(
    trim_some("
      TestOuter <- function() {
        actual <- lapply(
          input,
          function(x) {
            no_return()
          }
        )
        TestInner <- function() {
          no_return()
        }
        checkEquals(TestInner(), actual)
      }
    "),
    list(rex::rex("All functions must have an explicit return()."), line_number = 5L),
    linter
  )

  # capture group doesn't cause issues, #2678
  expect_no_lint(
    trim_some("
      TestFun <- function() {
        non_return()
      }
      AssertFun <- function() {
        non_return()
      }
    "),
    return_linter(return_style = "explicit", except_regex = "^(Test|Assert)")
  )
})

test_that("except= and except_regex= combination works", {
  expect_no_lint(
    trim_some("
      foo <- function() {
        no_return()
      }
      bar <- function() {
        no_return()
      }
      abaz <- function() {
        no_return()
      }
      bbaz <- function() {
        no_return()
      }
    "),
    return_linter(return_style = "explicit", except = c("foo", "bar"), except_regex = "baz$")
  )
})

test_that("return_linter skips brace-wrapped inline functions", { # nofuzz: comment_injection
  expect_no_lint("function(x) { sum(x) }", return_linter(return_style = "explicit"))
})

test_that("return_linter skips common S4 method functions", {
  linter <- return_linter(return_style = "explicit")

  expect_no_lint(
    trim_some("
      setGeneric(
        'ReadCircuitsPBAsDataTable',
        function(pbMessageList) {
          standardGeneric('ReadCircuitsPBAsDataTable')
        }
      )
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      setMethod('initialize', 'CircuitsTopology', function(.Object, ...) {
        callNextMethod(.Object, ...)
      })
    "),
    linter
  )
})

test_that("return_functions= is not affected by namespace qualification", {
  linter <- return_linter(return_style = "explicit", return_functions = "abort")

  expect_no_lint(
    trim_some("
      foo <- function(bar) {
        abort('bad')
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      foo <- function(bar) {
        rlang::abort('bad')
      }
    "),
    linter
  )
})

test_that("return_linter skips invokeRestart(), tryInvokeRestart()", {
  linter <- return_linter(return_style = "explicit")

  expect_no_lint(
    trim_some("
      warning = function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart('muffleWarning')
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      custom_warning = function(w) {
        warn <<- append(warn, conditionMessage(w))
        tryInvokeRestart('muffleCustom_warning')
      }
    "),
    linter
  )
})

# NB: x |> return() is blocked by the parser, so no need to test that.
test_that("Native pipes are handled correctly", {
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

test_that("return_linter handles arbitrarily nested terminal statements", {
  implicit_linter <- return_linter()
  implicit_msg <- rex::rex("Use implicit return behavior; explicit return() is not needed.")
  explicit_linter <- return_linter(return_style = "explicit")
  explicit_msg <- rex::rex("All functions must have an explicit return().")

  expect_no_lint(
    trim_some("
      foo <- function(x) {
        if (x < 0) {
          if (x == -1) {
            return(TRUE)
          }
          if (x > -10) {
            NA
          } else {
            FALSE
          }
        } else if (x == 0) {
          TRUE
        } else {
          y <- x**2
          if (y > 10) {
            z <- sin(y)
            if (z > 0) {
              FALSE
            } else {
              NA
            }
          } else {
            TRUE
          }
        }
      }
    "),
    implicit_linter
  )

  expect_no_lint(
    trim_some("
      foo <- function(x) {
        if (x < 0) {
          if (x == -1) {
            return(TRUE)
          }
          if (x > -10) {
            return(NA)
          } else {
            return(FALSE)
          }
        } else if (x == 0) {
          return(TRUE)
        } else {
          y <- x**2
          if (y > 10) {
            z <- sin(y)
            if (z > 0) {
              return(FALSE)
            } else {
              return(NA)
            }
          } else {
            return(TRUE)
          }
        }
      }
    "),
    explicit_linter
  )

  mixed_lines <- trim_some("
    foo <- function(x) {
      if (x < 0) {
        if (x == -1) {
          return(TRUE)
        }
        if (x > -10) {
          return(NA)
        } else {
          FALSE
        }
      } else if (x == 0) {
        return(TRUE)
      } else {
        y <- x**2
        if (y > 10) {
          z <- sin(y)
          if (z > 0) {
            FALSE
          } else {
            return(NA)
          }
        } else {
          TRUE
        }
      }
    }
  ")

  expect_lint(
    mixed_lines,
    list(
      list(implicit_msg, line_number = 7L),
      list(implicit_msg, line_number = 12L),
      list(implicit_msg, line_number = 20L)
    ),
    implicit_linter
  )

  expect_lint(
    mixed_lines,
    list(
      list(explicit_msg, line_number = 9L),
      list(explicit_msg, line_number = 18L),
      list(explicit_msg, line_number = 23L)
    ),
    explicit_linter
  )
})

test_that("explicit returns in control flow are linted correctly", {
  linter <- return_linter()
  lint_msg <- rex::rex("Use implicit return behavior")

  expect_lint(
    trim_some("
      foo <- function(bar) {
        if (TRUE) {
          return(bar)
        } else {
          return(NULL)
        }
      }
    "),
    list(lint_msg, lint_msg),
    linter
  )

  expect_no_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          if (TRUE) {
            return(1)
          }
          2
        } else {
          3
        }
      }
    "),
    linter
  )
})

# inspired by grid:::draw.all
#   https://github.com/r-devel/r-svn/blob/eeff859e427b2399f1474427a531365d2672f52f/src/library/grid/R/grob.R#L1940
test_that("logic is robust to absence of '{'", {
  linter <- return_linter()

  expect_no_lint(
    trim_some("
      foo <- function() {
        if (TRUE) # comment is a neighbor of 'if'
          FALSE
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      foo <- function() {
        if (TRUE)
          FALSE
        else # cannot rely on 'else' expr being e.g. 7th
          NA
      }
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          FALSE
        } else # cannot rely on 'else' expr being e.g. 7th
          NA
      }
    "),
    linter
  )
})

test_that("logic is robust to terminal comments under '{'", {
  implicit_linter <- return_linter()
  implicit_msg <- rex::rex("Use implicit return behavior; explicit return() is not needed.")
  explicit_linter <- return_linter(return_style = "explicit")
  explicit_msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      foo <- function() {
        return(TRUE)
        # comment
      }
    "),
    implicit_msg,
    implicit_linter
  )

  expect_no_lint(
    trim_some("
      foo <- function() {
        return(TRUE)
        # comment
      }
    "),
    explicit_linter
  )

  expect_lint(
    trim_some("
      foo <- function() {
        TRUE
        # comment
      }
    "),
    explicit_msg,
    explicit_linter
  )
})

test_that("terminal = assignment is not an error", {
  # key is this is not an <expr> node
  expect_no_lint(
    trim_some("
      foo <- function() {
        a = 1
      }
    "),
    return_linter()
  )
})

test_that("empty terminal '{' expression is handled correctly", {
  implicit_linter <- return_linter()
  implicit_msg <- rex::rex("Use implicit return behavior; explicit return() is not needed.")
  explicit_linter <- return_linter(return_style = "explicit")
  explicit_msg <- rex::rex("All functions must have an explicit return().")

  empty_inline <- "foo <- function() { }"
  expect_no_lint(empty_inline, implicit_linter)
  expect_no_lint(empty_inline, explicit_linter)

  empty_multiline <- trim_some("
    foo <- function() {
    }
  ")
  expect_no_lint(empty_multiline, implicit_linter)
  expect_no_lint(empty_multiline, explicit_linter)

  empty_comment <- trim_some("
    foo <- function() {
      # this line intentionally left blank
    }
  ")
  expect_no_lint(empty_comment, implicit_linter)
  expect_no_lint(empty_comment, explicit_linter)

  empty_if_implicit <- trim_some("
    foo <- function() {
      if (TRUE) {
      } else {
        FALSE
      }
    }
  ")
  expect_no_lint(empty_if_implicit, implicit_linter)
  expect_lint(
    empty_if_implicit,
    list(
      list(explicit_msg, line_number = 2L),
      list(explicit_msg, line_number = 4L)
    ),
    explicit_linter
  )

  empty_else_implicit <- trim_some("
    foo <- function() {
      if (TRUE) {
        FALSE
      } else {
      }
    }
  ")
  expect_no_lint(empty_else_implicit, implicit_linter)
  expect_lint(
    empty_else_implicit,
    list(
      list(explicit_msg, line_number = 3L),
      list(explicit_msg, line_number = 4L)
    ),
    explicit_linter
  )

  empty_if_explicit <- trim_some("
    foo <- function() {
      if (TRUE) {
      } else {
        return(FALSE)
      }
    }
  ")
  expect_lint(empty_if_explicit, list(implicit_msg, line_number = 4L), implicit_linter)
  expect_lint(empty_if_explicit, list(explicit_msg, line_number = 2L), explicit_linter)

  empty_else_explicit <- trim_some("
    foo <- function() {
      if (TRUE) {
        return(FALSE)
      } else {
      }
    }
  ")
  expect_lint(empty_else_explicit, list(implicit_msg, line_number = 3L), implicit_linter)
  expect_lint(empty_else_explicit, list(explicit_msg, line_number = 4L), explicit_linter)

  empty_if_else <- trim_some("
    foo <- function() {
      if (TRUE) {
      } else {
      }
    }
  ")
  expect_no_lint(empty_if_else, implicit_linter)
  expect_lint(
    empty_if_else,
    list(
      list(explicit_msg, line_number = 2L),
      list(explicit_msg, line_number = 3L)
    ),
    explicit_linter
  )

  weird <- trim_some("
    foo <- function() {
      if (TRUE) {{{{
        0
      }}}} else {
        { return(1) }
      }
    }
  ")
  expect_lint(weird, list(implicit_msg, line_number = 5L), implicit_linter)
  expect_lint(weird, list(explicit_msg, line_number = 3L), explicit_linter)
})

test_that("non-if returns are skipped under allow_implicit_else = FALSE", {
  expect_no_lint(
    trim_some("
      foo <- function(bar) {
        bar
      }
    "),
    return_linter(allow_implicit_else = FALSE)
  )
})

test_that("if/else don't throw a lint under allow_implicit_else = FALSE", {
  expect_no_lint(
    trim_some("
      foo <- function(bar) {
        if (TRUE) {
          bar
        } else {
          NULL
        }
      }
    "),
    return_linter(allow_implicit_else = FALSE)
  )
})

test_that("implicit else outside a function doesn't lint under allow_implicit_else = FALSE", {
  expect_no_lint("if(TRUE) TRUE", return_linter(allow_implicit_else = FALSE))
})

test_that("allow_implicit_else = FALSE identifies a simple implicit else", {
  expect_lint(
    trim_some("
      foo <- function(bar) {
        if (TRUE) {
          bar
        }
      }
    "),
    rex::rex("All functions with terminal if statements must have a corresponding terminal else clause"),
    return_linter(allow_implicit_else = FALSE)
  )
})

test_that("allow_implicit_else = FALSE finds implicit else with nested if+else", {
  lint_msg <- rex::rex("All functions with terminal if statements must have a corresponding terminal else clause")

  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          if (TRUE) {
            FALSE
          } else {
            TRUE
          }
        }
      }
    "),
    lint_msg,
    return_linter(allow_implicit_else = FALSE)
  )

  expect_lint(
    trim_some("
      foo <- function() {
        if (TRUE) {
          if (TRUE) {
            return(FALSE)
          } else {
            return(TRUE)
          }
        }
      }
    "),
    lint_msg,
    return_linter(return_style = "explicit", allow_implicit_else = FALSE)
  )
})

test_that("allow_implicit_else = FALSE works on anonymous/inline functions", {
  expect_lint(
    "lapply(rnorm(10), function(x) if (TRUE) x + 1)",
    rex::rex("All functions with terminal if statements must"),
    return_linter(allow_implicit_else = FALSE)
  )
})

test_that("side-effect functions like .onLoad ignore the lack of explicit else under allow_implicit_else = FALSE", {
  expect_no_lint(
    trim_some("
      .onAttach <- function(libname, pkgname) {
        if (TRUE) foo()
      }
    "),
    return_linter(allow_implicit_else = FALSE)
  )

  expect_no_lint(
    trim_some("
      .onAttach <- function(libname, pkgname) {
        if (TRUE) return(foo())
      }
    "),
    return_linter(return_style = "explicit", allow_implicit_else = FALSE)
  )
})

test_that("implicit else lint has the correct metadata", {
  linter <- return_linter(return_style = "explicit", allow_implicit_else = FALSE)
  lint_msg <- "All functions with terminal if statements"

  expect_lint(
    trim_some("
      foo <- function(x, y = 3) {
        if (x) {
          return(x)
        }
      }
    "),
    list(lint_msg, line_number = 2L),
    linter
  )

  expect_lint(
    trim_some("{
      foo <- function(x, y = 3) {
        if (x) {
          return(x)
        }
      }

      bar <- function(x, y = 3) {
        if (x) {
          return(x)
        }
      }

      baz <- function(x, y = 3) {
        if (x) return(x)
      }
    }"),
    list(
      list(lint_msg, line_number = 3L),
      list(lint_msg, line_number = 9L),
      list(lint_msg, line_number = 15L)
    ),
    linter
  )
})

test_that("Correct lints thrown when lacking explicit return and explicit else", {
  linter <- return_linter(return_style = "explicit", allow_implicit_else = FALSE)
  explicit_return_msg <- rex::rex("All functions must have an explicit return().")
  implicit_else_msg <- rex::rex("All functions with terminal if statements")

  expect_lint(
    trim_some("
      foo <- function(x, y = 3) {
        if (x) {
          x
        }
      }
    "),
    list(
      list(implicit_else_msg, line_number = 2L),
      list(explicit_return_msg, line_number = 3L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      function(x, y) {
        if (x) {
          1
        } else if (y) {
          2
        }
      }
    "),
    list(
      list(explicit_return_msg, line_number = 3L),
      list(implicit_else_msg, line_number = 4L),
      list(explicit_return_msg, line_number = 5L)
    ),
    linter
  )
})

test_that("Mixing exempted functions doesn't miss lints", {
  # in the current implementation, a local copy of 'params' is
  #   edited in a loop; this test ensures that behavior continues to be correct
  expect_lint(
    trim_some("{
      foo <- function() {
        1
      }

      bar <- function() {
        if (TRUE) {
          return(2)
        }
      }

      baz <- function() {
        if (TRUE) {
          3
        }
      }
    }"),
    list(
      list("Use implicit return behavior", line_number = 8L),
      list("All functions with terminal if statements", line_number = 13L)
    ),
    return_linter(allow_implicit_else = FALSE, except = "bar")
  )
})

test_that("= assignments are handled correctly", {
  implicit_linter <- return_linter(allow_implicit_else = FALSE)
  implicit_msg <- rex::rex("All functions with terminal if statements")
  explicit_linter <- return_linter(return_style = "explicit")
  explicit_msg <- rex::rex("All functions must have an explicit return().")

  expect_no_lint(
    trim_some("
      .onLoad = function() {
        1
      }
    "),
    explicit_linter
  )

  expect_no_lint(
    trim_some("
      .onLoad = function() {
        if (TRUE) 1
      }
    "),
    implicit_linter
  )

  expect_lint(
    trim_some("
      foo = function() {
        1
      }
    "),
    explicit_msg,
    explicit_linter
  )

  expect_lint(
    trim_some("
      foo = function() {
        if (TRUE) 1
      }
    "),
    implicit_msg,
    implicit_linter
  )
})

test_that("terminal switch() is handled correctly", {
  implicit_linter <- return_linter()
  implicit_msg <- rex::rex("Use implicit return behavior; explicit return() is not needed.")
  explicit_linter <- return_linter(return_style = "explicit")
  explicit_msg <- rex::rex("All functions must have an explicit return().")

  no_return_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = 1,
        b = 2
      )
    }
  ")
  expect_no_lint(no_return_lines, implicit_linter)
  expect_lint(no_return_lines, list(explicit_msg, explicit_msg), explicit_linter)

  outer_return_lines <- trim_some("
    foo <- function(x) {
      return(switch(x,
        a = 1,
        b = 2
      ))
    }
  ")
  expect_lint(outer_return_lines, implicit_msg, implicit_linter)
  expect_no_lint(outer_return_lines, explicit_linter)

  partial_return_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = return(1),
        b = 2
      )
    }
  ")
  expect_lint(partial_return_lines, implicit_msg, implicit_linter)
  expect_lint(partial_return_lines, explicit_msg, explicit_linter)

  all_return_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = return(1),
        b = return(2)
      )
    }
  ")
  expect_lint(all_return_lines, list(implicit_msg, implicit_msg), implicit_linter)
  expect_no_lint(all_return_lines, explicit_linter)

  default_all_return_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = return(1),
        return(2)
      )
    }
  ")
  expect_lint(default_all_return_lines, list(implicit_msg, implicit_msg), implicit_linter)
  expect_no_lint(default_all_return_lines, explicit_linter)

  default_no_return_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = 1,
        2
      )
    }
  ")
  expect_no_lint(default_no_return_lines, implicit_linter)
  expect_lint(default_no_return_lines, list(explicit_msg, explicit_msg), explicit_linter)

  no_return_braced_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = {
          1
          2
          3
          4
        },
        b = {
          5
          6
          7
        }
      )
    }
  ")
  expect_no_lint(no_return_braced_lines, implicit_linter)
  expect_lint(
    no_return_braced_lines,
    list(
      list(explicit_msg, line_number = 7L),
      list(explicit_msg, line_number = 12L)
    ),
    explicit_linter
  )

  all_return_braced_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = {
          1
          2
          3
          return(4)
        },
        b = {
          5
          6
          return(7)
        }
      )
    }
  ")
  expect_lint(
    all_return_braced_lines,
    list(
      list(implicit_msg, line_number = 7L),
      list(implicit_msg, line_number = 12L)
    ),
    implicit_linter
  )
  expect_no_lint(all_return_braced_lines, explicit_linter)

  early_return_braced_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = {
          1
          if (TRUE) {
            return(2)
          }
          3
          4
        },
        b = {
          5
          6
          7
        }
      )
    }
  ")
  expect_no_lint(early_return_braced_lines, implicit_linter)
  expect_lint(
    early_return_braced_lines,
    list(
      list(explicit_msg, line_number = 9L),
      list(explicit_msg, line_number = 14L)
    ),
    explicit_linter
  )

  if_no_return_braced_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = {
          1
          if (TRUE) {
            2
          } else {
            3
          }
        },
        b = {
          5
          6
          7
        }
      )
    }
  ")
  expect_no_lint(if_no_return_braced_lines, implicit_linter)
  expect_lint(
    if_no_return_braced_lines,
    list(
      list(explicit_msg, line_number = 6L),
      list(explicit_msg, line_number = 8L),
      list(explicit_msg, line_number = 14L)
    ),
    explicit_linter
  )

  if_return_braced_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = {
          1
          if (TRUE) {
            return(2)
          } else {
            return(3)
          }
        },
        b = {
          5
          6
          return(7)
        }
      )
    }
  ")
  expect_lint(
    if_return_braced_lines,
    list(
      list(implicit_msg, line_number = 6L),
      list(implicit_msg, line_number = 8L),
      list(implicit_msg, line_number = 14L)
    ),
    implicit_linter
  )
  expect_no_lint(if_return_braced_lines, explicit_linter)

  ok_exit_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = .Call(a_routine, x),
        b = .Call(b_routine, x),
        stop('invalid')
      )
    }
  ")
  expect_no_lint(ok_exit_lines, implicit_linter)
  expect_no_lint(ok_exit_lines, explicit_linter)
})

test_that("switch() default statements interact with allow_implicit_else", {
  implicit_linter <- return_linter(allow_implicit_else = FALSE)
  explicit_linter <- return_linter(allow_implicit_else = FALSE, return_style = "explicit")
  implicit_msg <- rex::rex("Use implicit return behavior; explicit return() is not needed.")
  explicit_msg <- rex::rex("All functions must have an explicit return().")
  implicit_switch_msg <- rex::rex("All functions with terminal switch statements")
  implicit_else_msg <- rex::rex("All functions with terminal if statements")

  no_default_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = 1,
        b = 2
      )
    }
  ")
  expect_lint(no_default_lines, list(implicit_switch_msg, line_number = 2L), implicit_linter)
  expect_lint(no_default_lines, list(implicit_switch_msg, explicit_msg, explicit_msg), explicit_linter)

  ifelse_default_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = 1,
        b = 2,
        if (x != 'c') {
          3
        } else {
          4
        }
      )
    }
  ")
  expect_no_lint(ifelse_default_lines, implicit_linter)
  expect_lint(ifelse_default_lines, list(explicit_msg, explicit_msg, explicit_msg, explicit_msg), explicit_linter)

  if_no_else_default_lines <- trim_some("
    foo <- function(x) {
      switch(x,
        a = 1,
        b = 2,
        if (x != 'c') {
          3
        }
      )
    }
  ")
  expect_lint(if_no_else_default_lines, list(implicit_else_msg, line_number = 5L), implicit_linter)
  expect_lint(
    if_no_else_default_lines,
    list(
      list(explicit_msg, line_number = 3L),
      list(explicit_msg, line_number = 4L),
      list(implicit_else_msg, line_number = 5L),
      list(explicit_msg, line_number = 6L)
    ),
    explicit_linter
  )
})

test_that("functions with braced expressions in formals lint correctly", {
  implicit_linter <- return_linter()
  implicit_msg <- rex::rex("Use implicit return behavior; explicit return() is not needed.")
  explicit_linter <- return_linter(return_style = "explicit")
  explicit_msg <- rex::rex("All functions must have an explicit return().")

  brace_lines_expl <- trim_some("
    foo <- function(y,
                    x = {
                      y <- sqrt(y)
                      y + 1
                    }) {
      return(x + y)
    }
  ")
  expect_lint(brace_lines_expl, implicit_msg, implicit_linter)
  expect_no_lint(brace_lines_expl, explicit_linter)

  brace_lines_impl <- trim_some("
    foo <- function(y,
                    x = {
                      y <- sqrt(y)
                      y + 1
                    }) {
      x + y
    }
  ")
  expect_no_lint(brace_lines_impl, implicit_linter)
  expect_lint(brace_lines_impl, explicit_msg, explicit_linter)

  lambda_lines_expl_expl <- trim_some("
    foo <- function(y,
                    F = function(z) {
                      return(z + 1)
                    }) {
      return(F(y) + y)
    }
  ")
  expect_lint(
    lambda_lines_expl_expl,
    list(
      list(implicit_msg, line_number = 3L),
      list(implicit_msg, line_number = 5L)
    ),
    implicit_linter
  )
  expect_no_lint(lambda_lines_expl_expl, explicit_linter)

  lambda_lines_expl_impl <- trim_some("
    foo <- function(y,
                    F = function(z) {
                      return(z + 1)
                    }) {
      F(y) + y
    }
  ")
  expect_lint(
    lambda_lines_expl_impl,
    list(implicit_msg, line_number = 3L),
    implicit_linter
  )
  expect_lint(
    lambda_lines_expl_impl,
    list(explicit_msg, line_number = 5L),
    explicit_linter
  )

  lambda_lines_impl_expl <- trim_some("
    foo <- function(y,
                    F = function(z) {
                      z + 1
                    }) {
      return(F(y) + y)
    }
  ")
  expect_lint(
    lambda_lines_impl_expl,
    list(implicit_msg, line_number = 5L),
    implicit_linter
  )
  expect_lint(
    lambda_lines_impl_expl,
    list(explicit_msg, line_number = 3L),
    explicit_linter
  )

  lambda_lines_impl_impl <- trim_some("
    foo <- function(y,
                    F = function(z) {
                      z + 1
                    }) {
      F(y) + y
    }
  ")
  expect_no_lint(lambda_lines_impl_impl, implicit_linter)
  expect_lint(
    lambda_lines_impl_impl,
    list(
      list(explicit_msg, line_number = 3L),
      list(explicit_msg, line_number = 5L)
    ),
    explicit_linter
  )
})
