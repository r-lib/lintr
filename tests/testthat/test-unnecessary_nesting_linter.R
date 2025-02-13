test_that("unnecessary_nesting_linter skips allowed usages", {
  linter <- unnecessary_nesting_linter()

  # parallel stops() and return()s are OK
  expect_lint(
    trim_some("
      if (A) {
        stop()
      } else {
        stop()
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (A) {
        return()
      } else {
        return()
      }
    "),
    NULL,
    linter
  )
})

test_that("Multiple if/else statements don't require unnesting", {
  # with further branches, reducing nesting might be less readable
  expect_lint(
    trim_some("
      if (x == 'a') {
        stop()
      } else if (x == 'b') {
        do_b()
      } else {
        stop()
      }
    "),
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("else-less if statements don't lint", {
  expect_lint(
    trim_some("
      if (x == 4) {
        msg <- 'failed'
        stop(msg)
      }
    "),
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("non-terminal expressions are not considered for the logic", {
  expect_lint(
    trim_some("
      if (x == 4) {
        x <- 5
        return(x)
      } else {
        return(x)
      }
    "),
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("parallels in further nesting are skipped", {
  expect_lint(
    trim_some("
      if (length(bucket) > 1) {
        return(age)
      } else {
        age <- age / 2
        if (grepl('[0-9]', age)) {
          return(age)
        } else {
          return('unknown')
        }
      }
    "),
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("unnecessary_nesting_linter blocks if/else with one exit branch", {
  linter <- unnecessary_nesting_linter()
  lint_msg <- rex::rex("Reduce the nesting of this if/else statement by unnesting the portion")

  expect_lint(
    trim_some("
      if (A) {
        stop()
      } else {
        B
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      if (A) {
        return()
      } else {
        B
      }
    "),
    lint_msg,
    linter
  )

  # also find exits in the later branch
  expect_lint(
    trim_some("
      if (A) {
        B
      } else {
        stop()
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      if (A) {
        B
      } else {
        return()
      }
    "),
    lint_msg,
    linter
  )

  stop_warning_lines <- trim_some("
    if (A) {
      stop('An error')
    } else {
      warning('A warning')
    }
  ")
  expect_lint(stop_warning_lines, lint_msg, linter)

  # Optionally consider 'warning' as an exit call --> no lint
  expect_lint(stop_warning_lines, NULL, unnecessary_nesting_linter(branch_exit_calls = "warning"))
})

test_that("unnecessary_nesting_linter skips one-line functions", {
  linter <- unnecessary_nesting_linter()

  expect_lint(
    trim_some("
      foo <- function(x) {
        return(x)
      }
    "),
    NULL,
    linter
  )

  # purrr anonymous functions also get skipped
  expect_lint(
    trim_some("
      purrr::map(x, ~ {
        .x
      })
    "),
    NULL,
    linter
  )
})

test_that("unnecessary_nesting_linter skips one-expression for loops", {
  linter <- unnecessary_nesting_linter()

  expect_lint(
    trim_some("
      for (i in 1:10) {
        print(i)
      }
    "),
    NULL,
    linter
  )

  # also for extended control flow functionality from packages
  expect_lint(
    trim_some("
      foreach (i = 1:10) %dopar% {
        print(i)
      }
    "),
    NULL,
    linter
  )
})

test_that("unnecessary_nesting_linter skips one-expression if and else clauses", {
  expect_lint(
    trim_some("
      if (TRUE) {
        x
      } else {
        y
      }
    "),
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("unnecessary_nesting_linter skips one-expression while loops", {
  expect_lint(
    trim_some("
      while (x < 10) {
        x <- x + 1
      }
    "),
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("unnecessary_nesting_linter skips one-expression repeat loops", {
  expect_lint(
    trim_some("
      repeat {
        x <- x + 1
      }
    "),
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("unnecessary_nesting_linter skips one-expression assignments by default", {
  expect_lint(
    trim_some("
      {
        x <- foo()
      }
    "),
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("unnecessary_nesting_linter passes for multi-line braced expressions", {
  expect_lint(
    trim_some("
      tryCatch(
        {
          foo(x)
          bar(x)
        },
        error = identity
      )
    "),
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("unnecessary_nesting_linter skips if unbracing won't reduce nesting", {
  linter <- unnecessary_nesting_linter()

  expect_lint(
    trim_some("
      test_that('this works', {
        expect_true(TRUE)
      })
    "),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
      DT[, {
        plot(x, y)
      }]
    "),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
      DT[, x := {
        foo(x, y)
      }]
    "),
    NULL,
    linter
  )

  # NB: styler would re-style these anyway
  expect_lint(
    trim_some("
      tryCatch({
        foo()
      }, error = identity)
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      DT[{
        n <- .N - 1
        x[n] < y[n]
      }, j = TRUE, by = x]
    "),
    NULL,
    linter
  )
})

test_that("rlang's double-brace operator is skipped", {
  expect_lint(
    "rename(DF, col = {{ val }})",
    NULL,
    unnecessary_nesting_linter()
  )
})

test_that("unnecessary_nesting_linter blocks one-expression braced expressions", {
  expect_lint(
    trim_some("
      tryToCatch(
        {
          foo(x)
        },
        error = identity
      )
    "),
    rex::rex("Reduce the nesting of this statement by removing the braces {}."),
    unnecessary_nesting_linter()
  )
})

test_that("unnecessary_nesting_linter allow_assignment= argument works", {
  expect_lint(
    trim_some("
      tryToCatch(
        {
          idx <- foo(x)
        },
        error = identity
      )
    "),
    rex::rex("Reduce the nesting of this statement by removing the braces {}."),
    unnecessary_nesting_linter(allow_assignment = FALSE)
  )
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Reduce the nesting of this if/else")

  expect_lint(
    trim_some("{
      if (A) {
        stop('no')
      } else {
        0
      }
      if (B) {
        stop('really no')
      } else {
        1
      }
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 7L)
    ),
    unnecessary_nesting_linter()
  )
})

test_that("unnecessary_nesting_linter skips allowed usages", {
  linter <- unnecessary_nesting_linter()

  expect_lint(
    trim_some("
      if (x && y) {
        1L
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      for (x in 1:3) {
        if (x && y) {
          1L
        }
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        1L
      } else if (y) {
        2L
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        1L
      } else {
        2L
        if (y) {
          3L
        }
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if (if (x) TRUE else FALSE) {
        1L
      }
    "),
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
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      if ((x && y) || (if (x) TRUE else FALSE)) {
        1L
      }
    "),
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
      }
    "),
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
      }
    "),
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
      }
    "),
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
      }
    "),
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
      }
    "),
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
      }
    "),
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
      }
    "),
    NULL,
    linter
  )
})

test_that("unnecessary_nesting_linter blocks disallowed usages", {
  lint_message <- rex::rex("Don't use nested `if` statements")
  linter <- unnecessary_nesting_linter()

  expect_lint(
    trim_some("
      if (x) {
        if (y) {
          1L
        }
      }
    "),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      if (x) {
        if (y) 1L
      }
    "),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      if (x && a) {
        if (y || b) {
          1L
        }
      }
    "),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      if (if (x) TRUE else FALSE) {
        if (y) {
          1L
        }
      }
    "),
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
      }
    "),
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
      }
    "),
    list(
      list(message = lint_message, line_number = 2L, column_number = 3L),
      list(message = lint_message, line_number = 3L, column_number = 5L)
    ),
    linter
  )
})

test_that("else that can drop braces is found", {
  linter <- unnecessary_nesting_linter()
  lint_msg <- rex::rex("Simplify this condition by using 'else if' instead of 'else { if'.")

  expect_lint(
    trim_some("
      if (A) {
        1
      } else {
        if (B) {
          2
        } else {
          3
        }
      }
    "),
    list(lint_msg, line_number = 4L),
    linter
  )

  expect_lint(
    trim_some("
      if (A) {
        1
      } else if (B) {
        2
      } else {
        if (C) {
          3
        } else {
          4
        }
      }
    "),
    list(lint_msg, line_number = 6L),
    linter
  )

  expect_lint(
    trim_some("
      if (A) {
        1
      } else {
        if (B) {
          2
        } else {
          if (C) {
            3
          } else {
            4
          }
        }
      }
    "),
    list(
      list(lint_msg, line_number = 4L),
      list(lint_msg, line_number = 7L)
    ),
    linter
  )
})

patrick::with_parameters_test_that(
  "default allowed functions are skipped",
  expect_lint(sprintf("%s(x, {y}, z)", call), NULL, unnecessary_nesting_linter()),
  call = c(
    "test_that", "with_parameters_test_that",
    "switch",
    "try", "tryCatch", "withCallingHandlers",
    "quote", "bquote", "expression", "substitute",
    "observe", "observeEvent", "reactive",
    "renderCachedPlot", "renderDataTable", "renderImage", "renderPlot",
    "renderPrint", "renderTable", "renderText", "renderUI"
  )
)

test_that("allow_functions= works", {
  linter_default <- unnecessary_nesting_linter()
  linter_foo <- unnecessary_nesting_linter(allow_functions = "foo")
  expect_lint("foo(x, {y}, z)", "Reduce the nesting of this statement", linter_default)
  expect_lint("foo(x, {y}, z)", NULL, linter_foo)
  expect_lint("test_that('a', {y})", NULL, linter_default)
  expect_lint("that_that('b', {y})", NULL, linter_foo)
})
