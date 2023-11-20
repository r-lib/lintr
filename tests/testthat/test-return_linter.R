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

test_that("Do not lint control statments (with return) on end of function", {
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
  msg <- rex::rex("All functions must have an explicit return().")

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
    list(
      line_number = 2L,
      message = msg
    ),
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
    list(
      line_number = 2L,
      message = msg
    ),
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
    list(
      line_number = 2L,
      message = msg
    ),
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
    list(
      line_number = 2L,
      message = msg
    ),
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

test_that("Do not lint stop on end of function", {
  msg <- rex::rex("All functions must have an explicit return().")

  expect_lint(
    trim_some("
      function(x) {
        switch(x, a = 1, 'b' = 2, '3' = 3, 4)
      }
    "),
    list(
      line_number = 2L,
      message = msg
    ),
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    trim_some("
      function(x) {
        switch(x, a = return(1), 'b' = stop(2), '3' = return(3), 4)
      }
    "),
    list(
      line_number = 2L,
      message = msg
    ),
    return_linter(use_implicit_returns = FALSE)
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
    list(
      line_number = 2L,
      message = msg
    ),
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    trim_some("
      function(x) {
        switch(x, a = return(1), 'b' = stop(2), '3' = return(3), stop('End'))
      }
    "),
    NULL,
    return_linter(use_implicit_returns = FALSE)
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
  expect_lint(
    trim_some("
      foo <- function(bar) {
        stop('bad')
      }
    "),
    NULL,
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    trim_some("
      foo <- function(bar) {
        stopifnot(bar == 'd')
      }
    "),
    NULL,
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("return_linter ignores expressions that aren't functions", {
  expect_lint(
    "x + 1", NULL, return_linter(use_implicit_returns = FALSE)
  )
})

test_that("return_linter ignores anonymous/inline functions", {
  lines <- "lapply(rnorm(10), function(x) x + 1)"
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter ignores if statements outside of functions", {
  lines <- c(
    "if(TRUE) {",
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
  lines <- c(
    "foo <- function() {",
    "  if(TRUE) {",
    "    TRUE",
    "  } else {",
    "    FALSE",
    "  }",
    "}"
  )
  expect_lint(
    lines,
    list(
      rex::rex("All functions must have an explicit return()."),
      rex::rex("All functions must have an explicit return().")
    ),
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("return_linter works regardless of braces in final if case", {
  lines <- c(
    "foo <- function() {",
    "  if(TRUE) TRUE",
    "}"
  )
  expect_lint(
    lines,
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
  )
  other_lines <- c(
    "foo <- function() {",
    "  if(TRUE) return(TRUE)",
    "}"
  )
  expect_lint(other_lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter finds missing return in one branch of an if", {
  lines <- c(
    "foo <- function() {",
    "  if(TRUE) {",
    "    return(TRUE)",
    "  } else {",
    "    FALSE",
    "  }",
    "}"
  )
  expect_lint(
    lines,
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
  )

  lines_other_way <- c(
    "foo <- function() {",
    "  if(TRUE) {",
    "    TRUE",
    "  } else {",
    "    return(FALSE)",
    "  }",
    "}"
  )
  expect_lint(
    lines_other_way,
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("return_linter works in nested if statements", {
  lines <- c(
    "foo <- function() {",
    "  if(TRUE) {",
    "    return(TRUE)",
    "  } else if (nzchar(\"a\")) {",
    "    return(TRUE)",
    "  } else {",
    "    return(FALSE)",
    "  }",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))

  more_lines <- c(
    "foo <- function() {",
    "  if(TRUE) {",
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
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("return_linter works in multi-line nested if statements", {
  lines <- c(
    "foo <- function() {",
    "  if(TRUE) {",
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
    return_linter(use_implicit_returns = FALSE)
  )

  other_lines <- c(
    "foo <- function() {",
    "  if(TRUE) {",
    "    if (nzchar(\"a\")) {",
    "      y <- 1 + 1",
    "      return(y)",
    "    }",
    "  } else {",
    "    return(FALSE)",
    "  }",
    "}"
  )
  expect_lint(other_lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter works for final for loops as well", {
  lines <- c(
    "foo <- function() {",
    "  for (i in seq_len(10)) {",
    "    if (i %% 2 == 0) {",
    "      y <- 1 + 1",
    "      return(y)",
    "    }",
    "  }",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))

  other_lines <- c(
    "foo <- function() {",
    "  for (i in seq_len(10)) {",
    "    if (i %% 2 == 0) {",
    "      y <- 1 + 1",
    "    }",
    "  }",
    "}"
  )
  expect_lint(
    other_lines,
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("return_linter works for function factories", {
  lines <- c(
    "foo <- function(x) {",
    "  function () {",
    "    return(x + 1)",
    "  }",
    "}"
  )
  expect_lint(
    lines,
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
  )

  failing_lines <- c(
    "foo <- function(x) {",
    "  function () {",
    "    x + 1",
    "  }",
    "}"
  )
  expect_lint(
    failing_lines,
    list(
      rex::rex("All functions must have an explicit return()."),
      rex::rex("All functions must have an explicit return().")
    ),
    return_linter(use_implicit_returns = FALSE)
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
    "  nativesupport::LoadNativeExtension()",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter correctly handles pipes", {
  lines <- c(
    "foo <- function(x) {",
    "  x %>%",
    "    return()",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))

  multiple_function_lines <- c(
    "foo <- function(x) {",
    "  x %>%",
    "    mean() %>%",
    "    return()",
    "}"
  )
  expect_lint(multiple_function_lines, NULL, return_linter(use_implicit_returns = FALSE))

  preceding_pipe_lines <- c(
    "foo <- function(x) {",
    "  y <- rnorm(length(x))",
    "",
    "  x %>%",
    "    cbind(y) %>%",
    "    return()",
    "}"
  )
  expect_lint(preceding_pipe_lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter handles pipes in control flow", {
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
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))

  bad_lines <- c(
    "foo <- function(x) {",
    "  for (i in seq_len(10)) {",
    "    x %>%",
    "      mean()",
    "  }",
    "}"
  )
  expect_lint(
    bad_lines,
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
  )

  missing_branch_lines <- c(
    "foo <- function(x) {",
    "  if (TRUE) {",
    "    x %>%",
    "      mean()",
    "  } else {",
    "    return(TRUE)",
    "  }",
    "}"
  )
  expect_lint(
    missing_branch_lines,
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
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
  setup_lines <- c(
    ".setUp <- function() {",
    "  options(foo = TRUE)",
    "}"
  )
  expect_lint(setup_lines, NULL, return_linter(use_implicit_returns = FALSE))

  teardown_lines <- c(
    ".tearDown <- function() {",
    "  options(foo = TRUE)",
    "}"
  )
  expect_lint(teardown_lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter allows RUnit tests to pass", {
  lines <- c(
    "TestKpSxsSummary <- function() {",
    "  context <- foo(72643424)",
    "  expected <- data.frame(a = 2)",
    "  checkEquals(expected, bar(context))",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))

  custom_lines <- c(
    "TestMyPackage <- function() {",
    "  checkMyCustomComparator(x, y)",
    "}"
  )
  expect_lint(custom_lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter skips RUnit functions in argumented tests", {
  lines <- c(
    "TestKpSxsSummary <- function(an_argument) {",
    "  context <- foo(an_argument)",
    "  expected <- data.frame(a = 2)",
    "  checkEquals(expected, bar(context))",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter skips terminal LOG and logging::LOG", {
  lines <- c(
    "foo <- function(bar) {",
    "  LOG('INFO', 'bad')",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))

  ns_lines <- c(
    "foo <- function(bar) {",
    "  logging::LOG('INFO', 'bad')",
    "}"
  )
  expect_lint(ns_lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter skips brace-wrapped inline functions", {
  expect_lint("function(x) { sum(x) }", NULL, return_linter(use_implicit_returns = FALSE))
})

# b/194585283
test_that("return_linter skips common S4 method functions", {
  lines_standard_generic <- c(
    "setGeneric(",
    '  "ReadCircuitsPBAsDataTable",',
    "  function(pbMessageList) {",
    '    standardGeneric("ReadCircuitsPBAsDataTable")',
    "  }",
    ")"
  )
  expect_lint(lines_standard_generic, NULL, return_linter(use_implicit_returns = FALSE))

  lines_call_next_method <- c(
    'setMethod("initialize", "CircuitsTopology", function(.Object, ...) {',
    "  callNextMethod(.Object, ...)",
    "})"
  )
  expect_lint(lines_call_next_method, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter skips rlang::abort", {
  lines <- c(
    "foo <- function(bar) {",
    "  abort('bad')",
    "}"
  )
  expect_lint(lines, NULL, return_linter(use_implicit_returns = FALSE))

  ns_lines <- c(
    "foo <- function(bar) {",
    "  rlang::abort('bad')",
    "}"
  )
  expect_lint(ns_lines, NULL, return_linter(use_implicit_returns = FALSE))
})

test_that("return_linter skips invokeRestart(), tryInvokeRestart()", {
  invoke_lines <- c(
    "warning = function(w) {",
    "  warn <<- append(warn, conditionMessage(w))",
    '  invokeRestart("muffleWarning")',
    "}"
  )
  expect_lint(invoke_lines, NULL, return_linter(use_implicit_returns = FALSE))

  try_invoke_lines <- c(
    "custom_warning = function(w) {",
    "  warn <<- append(warn, conditionMessage(w))",
    '  tryInvokeRestart("muffleCustom_warning")',
    "}"
  )
  expect_lint(try_invoke_lines, NULL, return_linter(use_implicit_returns = FALSE))
})

# NB: x |> return() is blocked by the parser, so no need to test that.
test_that("Native pipes are handled correctly", {
  expect_lint(
    c(
      "foo <- function(x) {",
      "  for (i in seq_len(10)) {",
      "    x |>",
      "      mean()",
      "  }",
      "}"
    ),
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
  )

  expect_lint(
    c(
      "foo <- function(x) {",
      "  if (TRUE) {",
      "    x |>",
      "      mean()",
      "  } else {",
      "    return(TRUE)",
      "  }",
      "}"
    ),
    rex::rex("All functions must have an explicit return()."),
    return_linter(use_implicit_returns = FALSE)
  )
})

test_that("return_linter works for final while/repeat loops as well", {
  while_lines <- c(
    "foo <- function(x) {",
    "  while (x > 0) {",
    "    if (x %% 2 == 0) {",
    "      return(x)",
    "    }",
    "    x <- x + sample(10, 1)",
    "  }",
    "}"
  )
  expect_lint(while_lines, NULL, return_linter(use_implicit_returns = FALSE))

  repeat_lines <- c(
    "foo <- function(x) {",
    "  repeat {",
    "    if (x == 0) {",
    "      return(x)",
    "    }",
    "    x <- x - sign(x)",
    "  }",
    "}"
  )
  expect_lint(repeat_lines, NULL, return_linter(use_implicit_returns = FALSE))
})
