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
      message = rex::rex("Use implicit return behavior; explicit return() is not needed.")
    ),
    return_linter()
  )
})

test_that("Do not lint if/else statements (with return) on end of function", {
  linter <- return_linter(use_implicit_returns = FALSE)

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
  linter <- return_linter(use_implicit_returns = FALSE)
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

test_that("Do not lint stop on end of function", {
  linter <- return_linter(use_implicit_returns = FALSE)
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
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("return_linter works for using stop() instead of returning", {
  linter <- return_linter(use_implicit_returns = FALSE)

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
  expect_lint("x + 1", NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter ignores anonymous/inline functions", {
  lines <- "lapply(rnorm(10), function(x) x + 1)"
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter ignores if statements outside of functions", {
  lines <- c(
    "if (TRUE) {",
    "  TRUE",
    "} else {",
    "  FALSE",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter passes on multi-line functions", {
  lines <- c(
    "foo <- function(x) {",
    "  y <- x + 1",
    "  return(y)",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))
})


test_that("return_linter identifies a simple missing return", {
  expect_lint(
    trim_some("
      foo <- function(bar) {
        bar
      }
    "),
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
  )
})


test_that("return_linter finds a missing return in a 2+ line function", {
  lines <- c(
    "foo <- function(x) {",
    "  y <- x + 1",
    "  y^2",
    "}"
  )
  expect_lint(
    lines,
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("return_linter finds a missing return despite early returns", {
  lines <- c(
    "foo <- function(x) {",
    "  if (TRUE) return(TRUE)",
    "  x <- 1 + 1",
    "  x",
    "}"
  )
  expect_lint(
    lines,
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
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
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("return_linter works regardless of braces in final if case", {
  linter <- return_linter(use_implicit_returns = FALSE)

  lines <- c(
    "foo <- function() {",
    "  if (TRUE) TRUE",
    "}"
  )
  expect_lint(
    lines,
    rex::rex("All functions must have an explicit return()."),
    linter
  )
  other_lines <- c(
    "foo <- function() {",
    "  if (TRUE) return(TRUE)",
    "}"
  )
  expect_lint(other_lines, NULL, linter)
})

test_that("return_linter finds missing return in one branch of an if", {
  linter <- return_linter(use_implicit_returns = FALSE)
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
  linter <- return_linter(use_implicit_returns = FALSE)

  lines <- c(
    "foo <- function() {",
    "  if (TRUE) {",
    "    return(TRUE)",
    "  } else if (nzchar(\"a\")) {",
    "    return(TRUE)",
    "  } else {",
    "    return(FALSE)",
    "  }",
    "}"
  )
  expect_lint(lines, NULL, linter)

  more_lines <- c(
    "foo <- function() {",
    "  if (TRUE) {",
    "    if (nzchar(\"a\")) {",
    "      TRUE",
    "    }",
    "  } else {",
    "    return(FALSE)",
    "  }",
    "}"
  )
  expect_lint(
    more_lines,
    rex::rex("All functions must have an explicit return()."),
    linter
  )
})

test_that("return_linter works in multi-line nested if statements", {
  linter <- return_linter(use_implicit_returns = FALSE)

  lines <- c(
    "foo <- function() {",
    "  if (TRUE) {",
    "    if (nzchar(\"a\")) {",
    "      y <- 1 + 1",
    "      y",
    "    }",
    "  } else {",
    "    return(FALSE)",
    "  }",
    "}"
  )
  expect_lint(
    lines,
    rex::rex("All functions must have an explicit return()."),
    linter
  )

  other_lines <- c(
    "foo <- function() {",
    "  if (TRUE) {",
    "    if (nzchar(\"a\")) {",
    "      y <- 1 + 1",
    "      return(y)",
    "    }",
    "  } else {",
    "    return(FALSE)",
    "  }",
    "}"
  )
  expect_lint(other_lines, NULL, linter)
})

test_that("return_linter works for final for loops as well", {
  linter <- return_linter(use_implicit_returns = FALSE)
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
  linter <- return_linter(use_implicit_returns = FALSE)
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
  lines <- c(
    "ReadCapacitorAsList <- function(file) {",
    "  .Call(R_ReadCapacitorAsList, file)",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter allows return()-less namespace hook calls", {
  lines <- c(
    ".onLoad <- function(libname, pkgname) {",
    "  do_setup()",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter correctly handles pipes", {
  linter <- return_linter(use_implicit_returns = FALSE)

  lines <- c(
    "foo <- function(x) {",
    "  x %>%",
    "    return()",
    "}"
  )
  expect_lint(lines, NULL, linter)

  multiple_function_lines <- c(
    "foo <- function(x) {",
    "  x %>%",
    "    mean() %>%",
    "    return()",
    "}"
  )
  expect_lint(multiple_function_lines, NULL, linter)

  preceding_pipe_lines <- c(
    "foo <- function(x) {",
    "  y <- rnorm(length(x))",
    "",
    "  x %>%",
    "    cbind(y) %>%",
    "    return()",
    "}"
  )
  expect_lint(preceding_pipe_lines, NULL, linter)
})

test_that("return_linter handles pipes in control flow", {
  linter <- return_linter(use_implicit_returns = FALSE)
  lint_msg <- rex::rex("All functions must have an explicit return().")

  lines <- c(
    "foo <- function(x) {",
    "  if (TRUE) {",
    "    return(invisible())",
    "  } else {",
    "    x %>%",
    "      return()",
    "  }",
    "}"
  )
  expect_lint(lines, NULL, linter)

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
  lines <- c(
    "foo <- function(x) {",
    "  if (TRUE) {",
    "    q('n')",
    "  } else {",
    "    quit('n')",
    "  }",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter passes on .setUp/.tearDown calls", {
  linter <- return_linter(use_implicit_returns = FALSE, use_runit = TRUE)

  setup_lines <- c(
    ".setUp <- function() {",
    "  options(foo = TRUE)",
    "}"
  )
  expect_lint(setup_lines, NULL, linter)

  teardown_lines <- c(
    ".tearDown <- function() {",
    "  options(foo = TRUE)",
    "}"
  )
  expect_lint(teardown_lines, NULL, linter)
})

test_that("return_linter allows RUnit tests to pass", {
  linter <- return_linter(use_implicit_returns = FALSE, use_runit = TRUE)

  lines <- c(
    "TestKpSxsSummary <- function() {",
    "  context <- foo(72643424)",
    "  expected <- data.frame(a = 2)",
    "  checkEquals(expected, bar(context))",
    "}"
  )
  expect_lint(lines, NULL, linter)

  custom_lines <- c(
    "TestMyPackage <- function() {",
    "  checkMyCustomComparator(x, y)",
    "}"
  )
  expect_lint(custom_lines, NULL, linter)
})

test_that("return_linter skips RUnit functions in argumented tests", {
  lines <- c(
    "TestSummary <- function(an_argument) {",
    "  context <- foo(an_argument)",
    "  expected <- data.frame(a = 2)",
    "  checkEquals(expected, bar(context))",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE, use_runit = TRUE))
})

test_that("return_linter accepts additional allowed functions I", {
  linter <- return_linter(use_implicit_returns = FALSE, additional_allowed_func = "LOG")

  lines <- c(
    "foo <- function(bar) {",
    "  LOG('INFO', 'bad')",
    "}"
  )
  expect_lint(lines, NULL, linter)

  ns_lines <- c(
    "foo <- function(bar) {",
    "  logging::LOG('INFO', 'bad')",
    "}"
  )
  expect_lint(ns_lines, NULL, linter)
})

test_that("return_linter accepts additional side effect functions", {
  linter <- return_linter(use_implicit_returns = FALSE, additional_side_effect_func = "foo")

  lines <- c(
    "foo <- function(bar) {",
    "  5 +3",
    "}"
  )
  expect_lint(lines, NULL, linter)
})

test_that("return_linter skips brace-wrapped inline functions", {
  expect_lint("function(x) { sum(x) }", NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter skips common S4 method functions", {
  linter <- return_linter(use_implicit_returns = FALSE)

  lines_standard_generic <- c(
    "setGeneric(",
    '  "ReadCircuitsPBAsDataTable",',
    "  function(pbMessageList) {",
    '    standardGeneric("ReadCircuitsPBAsDataTable")',
    "  }",
    ")"
  )
  expect_lint(lines_standard_generic, NULL, linter)

  lines_call_next_method <- c(
    'setMethod("initialize", "CircuitsTopology", function(.Object, ...) {',
    "  callNextMethod(.Object, ...)",
    "})"
  )
  expect_lint(lines_call_next_method, NULL, linter)
})

test_that("return_linter accepts additional allowed functions II", {
  linter <- return_linter(use_implicit_returns = FALSE, additional_allowed_func = "abort")

  lines <- c(
    "foo <- function(bar) {",
    "  abort('bad')",
    "}"
  )
  expect_lint(lines, NULL, linter)

  ns_lines <- c(
    "foo <- function(bar) {",
    "  rlang::abort('bad')",
    "}"
  )
  expect_lint(ns_lines, NULL, linter)
})

test_that("return_linter skips invokeRestart(), tryInvokeRestart()", {
  linter <- return_linter(use_implicit_returns = FALSE)

  invoke_lines <- c(
    "warning = function(w) {",
    "  warn <<- append(warn, conditionMessage(w))",
    '  invokeRestart("muffleWarning")',
    "}"
  )
  expect_lint(invoke_lines, NULL, linter)

  try_invoke_lines <- c(
    "custom_warning = function(w) {",
    "  warn <<- append(warn, conditionMessage(w))",
    '  tryInvokeRestart("muffleCustom_warning")',
    "}"
  )
  expect_lint(try_invoke_lines, NULL, linter)
})

# NB: x |> return() is blocked by the parser, so no need to test that.
test_that("Native pipes are handled correctly", {
  skip_if_not_r_version("4.1.0")

  linter <- return_linter(use_implicit_returns = FALSE)
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
  linter <- return_linter(use_implicit_returns = FALSE)
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
  linter <- return_linter(use_implicit_returns = FALSE)
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