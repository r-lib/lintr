test_that("unreachable_code_linter works in simple function", {
  lines <- trim_some("
    foo <- function(bar) {
      return(bar)
    }
  ")
  expect_lint(lines, NULL, unreachable_code_linter())
})

test_that("unreachable_code_linter ignores expressions that aren't functions", {
  expect_lint("x + 1", NULL, unreachable_code_linter())
})

test_that("unreachable_code_linter ignores anonymous/inline functions", {
  expect_lint("lapply(rnorm(10), function(x) x + 1)", NULL, unreachable_code_linter())
})

test_that("unreachable_code_linter passes on multi-line functions", {
  lines <- trim_some("
    oo <- function(x) {
      y <- x + 1
      return(y)
    }
  ")
  expect_lint(lines, NULL, unreachable_code_linter())
})

test_that("unreachable_code_linter ignores comments on the same expression", {
  lines <- trim_some("
    foo <- function(x) {
      return(
        y^2
      ) # y^3
    }
  ")
  expect_lint(lines, NULL, unreachable_code_linter())
})

test_that("unreachable_code_linter ignores comments on the same line", {
  lines <- trim_some("
    foo <- function(x) {
      return(y^2) # y^3
    }
  ")
  expect_lint(lines, NULL, unreachable_code_linter())
})

test_that("unreachable_code_linter identifies simple unreachable code", {
  lines <- trim_some("
    foo <- function(bar) {
      return(bar)
      x + 3
    }
  ")
  # testing the correct expression is linted (the first culprit line)
  expect_lint(
    lines,
    list(
      line_number = 3L,
      message = rex::rex("Code and comments coming after a top-level return() or stop()")
    ),
    unreachable_code_linter()
  )
})

test_that("unreachable_code_linter finds unreachable comments", {
  lines <- trim_some("
    foo <- function(x) {
      y <- x + 1
      return(y^2)
      # y^3
    }
  ")
  expect_lint(
    lines,
    rex::rex("Code and comments coming after a top-level return() or stop()"),
    unreachable_code_linter()
  )
})

test_that("unreachable_code_linter finds expressions in the same line", {
  msg <- rex::rex("Code and comments coming after a top-level return() or stop()")
  linter <- unreachable_code_linter()

  lines <- trim_some("
    foo <- function(x) {
      return(
        y^2
      ); 3 + 1
    }
  ")
  expect_lint(lines, msg, linter)

  lines <- trim_some("
    foo <- function(x) {
      return(y^2); 3 + 1
    }
  ")
  expect_lint(lines, msg, linter)

  lines <- trim_some("
    foo <- function(x) {
      return(y^2); 3 + 1 # Test
    }
  ")
  expect_lint(lines, msg, linter)
})

test_that("unreachable_code_linter finds expressions and comments after comment in return line", {
  msg <- rex::rex("Code and comments coming after a top-level return() or stop()")
  linter <- unreachable_code_linter()

  lines <- trim_some("
    foo <- function(x) {
      return(y^2) #Test comment
      #Test comment 2
    }
  ")
  expect_lint(lines, msg, linter)

  lines <- trim_some("
    foo <- function(x) {
      return(y^2) # Test
      3 + 1
    }
  ")
  expect_lint(lines, msg, linter)
})

test_that("unreachable_code_linter finds a double return", {
  lines <- trim_some("
    foo <- function(x) {
      return(y^2)
      return(y^3)
    }
  ")
  expect_lint(
    lines,
    rex::rex("Code and comments coming after a top-level return() or stop()"),
    unreachable_code_linter()
  )
})

test_that("unreachable_code_linter finds code after stop()", {
  lines <- trim_some("
    foo <- function(x) {
      y <- x + 1
      stop(y^2)
      # y^3
    }
  ")
  expect_lint(
    lines,
    rex::rex("Code and comments coming after a top-level return() or stop()"),
    unreachable_code_linter()
  )
})

test_that("unreachable_code_linter ignores code after foo$stop(), which might be stopping a subprocess, for example", {
  expect_lint(
    trim_some("
      foo <- function(x) {
        bar <- get_process()
        bar$stop()
        TRUE
      }
    "),
    NULL,
    unreachable_code_linter()
  )
  expect_lint(
    trim_some("
      foo <- function(x) {
        bar <- get_process()
        bar@stop()
        TRUE
      }
    "),
    NULL,
    unreachable_code_linter()
  )
})

test_that("unreachable_code_linter ignores terminal nolint end comments", {
  withr::local_options(list(
    lintr.exclude_start = "#\\s*TestNoLintStart",
    lintr.exclude_end = "#\\s*TestNoLintEnd"
  ))
  expect_lint(
    trim_some("
      foo <- function() {
        do_something
        # TestNoLintStart: one_linter.
        a = 42
        return(a)
        # TestNoLintEnd
      }
    "),
    NULL,
    list(unreachable_code_linter(), one_linter = assignment_linter())
  )
})

test_that("unreachable_code_linter identifies unreachable code in conditional loops", {
  linter <- unreachable_code_linter()
  msg <- rex::rex("Code inside a conditional loop with a deterministically false condition should be removed.")

  lines <- trim_some("
    foo <- function(bar) {
      if (FALSE) {
        x <- 3
      }
      x + 3
    }
  ")

  expect_lint(lines, list(line_number = 3L, message = msg), linter)

  lines <- trim_some("
    foo <- function(bar) {
      if (FALSE) {
        # Unlinted comment
        x <- 3
      }
      x + 3
    }
  ")

  expect_lint(lines, list(line_number = 4L, message = msg), linter)

  lines <- trim_some("
    foo <- function(bar) {
      if (bla) {
        x <- 3
      } else if (FALSE) {
        # Unlinted comment
        y <- 3
      }
      x + 3
    }
  ")

  expect_lint(lines, list(line_number = 6L, message = msg), linter)

  lines <- trim_some("
    foo <- function(bar) {
      while (FALSE) {
        x <- 3
      }
      x + 3
    }
  ")

  expect_lint(lines, list(line_number = 3L, message = msg), linter)

  lines <- trim_some("
    foo <- function(bar) {
      while (FALSE) {
        # Unlinted comment
        x <- 3
      }
      x + 3
    }
  ")

  expect_lint(lines, list(line_number = 4L, message = msg), linter)

  lines <- "while (FALSE) x <- 3"

  expect_lint(
    lines,
    list(line_number = 1L, ranges = list(c(15L, 20L)), message = msg),
    linter
  )

  lines <- "if (FALSE) x <- 3 # Test comment"

  expect_lint(
    lines,
    list(line_number = 1L, ranges = list(c(12L, 17L)), message = msg),
    linter
  )
})

test_that("unreachable_code_linter identifies unreachable code in conditional loops", {
  linter <- unreachable_code_linter()
  msg <- rex::rex("Code inside an else block after a deterministically true if condition should be removed.")

  lines <- trim_some("
    foo <- function(bar) {
      if (TRUE) {
        x <- 3
      } else {
        # Unlinted comment
        x + 3
      }
    }
  ")

  expect_lint(lines, list(line_number = 6L, message = msg), linter)

  lines <- trim_some("
    foo <- function(bar) {
      if (TRUE) {
        x <- 3
      } else if (bar) {
        # Unlinted comment
        x + 3
      }
    }
  ")

  expect_lint(lines, list(line_number = 4L, message = msg), linter)

  lines <- "if (TRUE) x <- 3 else if (bar) x + 3"

  expect_lint(
    lines,
    list(line_number = 1L, ranges = list(c(23L, 36L)), message = msg),
    linter
  )
})

test_that("unreachable_code_linter identifies unreachable code in mixed conditional loops", {
  linter <- unreachable_code_linter()
  msg <- rex::rex("Code inside a conditional loop with a deterministically false condition should be removed.")

  lines <- trim_some("
    function (bla) {
      if (FALSE) {
        code + 4
      }
      while (FALSE) {
        code == 3
      }
      if (TRUE) {
      } else {
        code + bla
      }
      stop('.')
      code <- 1
    }
  ")

  expect_lint(
    lines,
    list(
      list(line_number = 3L, message = msg),
      list(line_number = 6L, message = msg),
      list(
        line_number = 10L,
        message = rex::rex("Code inside an else block after a deterministically true if condition should be removed.")
      ),
      list(
        line_number = 13L,
        message = rex::rex("Code and comments coming after a top-level return() or stop()")
      )
    ),
    linter
  )

  lines <- "if (FALSE) x <- 3 else if (TRUE) x + 3 else x + 4"

  expect_lint(
    lines,
    list(
      list(line_number = 1L, ranges = list(c(12L, 17L)), message = msg),
      list(
        line_number = 1L,
        ranges = list(c(45L, 49L)),
        message = rex::rex("Code inside an else block after a deterministically true if condition should be removed.")
      )
    ),
    linter
  )
})

test_that("function shorthand is handled", {
  expect_lint(
    trim_some("
      foo <- \\(bar) {
        return(bar)
        x + 3
      }
    "),
    list(
      line_number = 3L,
      message = rex::rex("Code and comments coming after a top-level return() or stop()")
    ),
    unreachable_code_linter()
  )
})

# nolint start: commented_code_linter.
# TODO(michaelchirico): extend to work on switch() statements
# test_that("unreachable_code_linter interacts with switch() as expected", {
#   unreachable_inside_switch_lines <- trim_some("
#     foo <- function(x) {
#       switch(x,
#         a = {
#           return(x)
#           x + 1
#         },
#         b = {
#           return(x + 1)
#         }
#       )
#     }
#   ")
#   expect_lint(
#     unreachable_inside_switch_lines,
#     rex::rex("Code and comments coming after a top-level return() or stop()"),
#     unreachable_code_linter()
#   )
# })
# nolint end: commented_code_linter.

# TODO(michaelchirico): the logic could be extended to terminal if statements
#   or control flows (for/while). There shouldn't really be such a thing as
#   a terminal for/while (owing to ExplicitReturnLinter forcing these to
#   be followed by return(invisible()) or similar), but could be included to
#   catch comments for completeness / robustness as a standalone function.
#   Terminal if statements are a bit messy, but would have some payoff.
# TODO(michaelchirico): again similarly, this could also apply to cases without
#   explicit returns (where it can only apply to comments)
