test_that("unreachable_code_linter works in simple function", {
  lines <- trim_some("
    foo <- function(bar) {
      return(bar)
    }
  ")
  expect_lint(lines, NULL, unreachable_code_linter())
})

test_that("unreachable_code_linter works in sub expressions", {
  linter <- unreachable_code_linter()
  msg <- rex::rex("Code and comments coming after a return() or stop()")

  lines <- trim_some("
    foo <- function(bar) {
      if (bar) {
        return(bar)
        # Test comment
        while (bar) {
          return(bar)
          5 + 3
          repeat {
            return(bar)
            # Test comment
          }
        }
      } else if (bla) {
        # test
        return(5)
        # Test 2
      } else {
        return(bar)
        # Test comment
        for(i in 1:3) {
          return(bar)
          5 + 4
        }
      }
      return(bar)
      5 + 1
    }
  ")

  expect_lint(
    lines,
    list(
      list(line_number = 4L, message = msg),
      list(line_number = 7L, message = msg),
      list(line_number = 10L, message = msg),
      list(line_number = 16L, message = msg),
      list(line_number = 19L, message = msg),
      list(line_number = 22L, message = msg),
      list(line_number = 26L, message = msg)
    ),
    linter
  )

  lines <- trim_some("
    foo <- function(bar) {
      if (bar) {
        return(bar) # Test comment
      }
      while (bar) {
        return(bar) # 5 + 3
      }
      repeat {
        return(bar) # Test comment
      }

    }
  ")

  expect_lint(lines, NULL, linter)

  lines <- trim_some("
    foo <- function(bar) {
      if (bar) {
        return(bar); x <- 2
      } else {
        return(bar); x <- 3
      }
      while (bar) {
        return(bar); 5 + 3
      }
      repeat {
        return(bar); test()
      }
      for(i in 1:3) {
        return(bar); 5 + 4
      }
    }
  ")

  expect_lint(
    lines,
    list(
      list(line_number = 3L, message = msg),
      list(line_number = 5L, message = msg),
      list(line_number = 8L, message = msg),
      list(line_number = 11L, message = msg),
      list(line_number = 14L, message = msg)
    ),
    linter
  )
})

test_that("unreachable_code_linter works with next and break in sub expressions", {
  linter <- unreachable_code_linter()
  msg <- rex::rex("Code and comments coming after a `next` or `break`")

  lines <- trim_some("
    foo <- function(bar) {
      if (bar) {
        next
        # Test comment
        while (bar) {
          break
          5 + 3
          repeat {
            next
            # Test comment
          }
        }
      } else {
        next
        # test
        for(i in 1:3) {
          break
          5 + 4
        }
      }
    }
  ")

  expect_lint(
    lines,
    list(
      list(line_number = 4L, message = msg),
      list(line_number = 7L, message = msg),
      list(line_number = 10L, message = msg),
      list(line_number = 15L, message = msg),
      list(line_number = 18L, message = msg)
    ),
    linter
  )

  lines <- trim_some("
    foo <- function(bar) {
      if (bar) {
        break # Test comment
      } else {
        next # Test comment
      }
      while (bar) {
        next # 5 + 3
      }
      repeat {
        next # Test comment
      }
      for(i in 1:3) {
        break # 5 + 4
      }
    }
  ")

  expect_lint(lines, NULL, linter)

  lines <- trim_some("
    foo <- function(bar) {
      if (bar) {
        next; x <- 2
      } else {
        break; x <- 3
      }
      while (bar) {
        break; 5 + 3
      }
      repeat {
        next; test()
      }
      for(i in 1:3) {
        break; 5 + 4
      }
    }
  ")

  expect_lint(
    lines,
    list(
      list(line_number = 3L, message = msg),
      list(line_number = 5L, message = msg),
      list(line_number = 8L, message = msg),
      list(line_number = 11L, message = msg),
      list(line_number = 14L, message = msg)
    ),
    linter
  )
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
      message = rex::rex("Code and comments coming after a return() or stop()")
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
    rex::rex("Code and comments coming after a return() or stop()"),
    unreachable_code_linter()
  )
})

test_that("unreachable_code_linter finds expressions in the same line", {
  msg <- rex::rex("Code and comments coming after a return() or stop()")
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
  msg <- rex::rex("Code and comments coming after a return() or stop()")
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
    rex::rex("Code and comments coming after a return() or stop()"),
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
    rex::rex("Code and comments coming after a return() or stop()"),
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

  expect_lint(
    trim_some("
      foo <- function() {
        do_something
        # nolint start: one_linter.
        a = 42
        next
        # nolint end
      }
    "),
    NULL,
    unreachable_code_linter()
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
        message = rex::rex("Code and comments coming after a return() or stop()")
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
  skip_if_not_r_version("4.1.0")

  expect_lint(
    trim_some("
      foo <- \\(bar) {
        return(bar)
        x + 3
      }
    "),
    list(
      line_number = 3L,
      message = rex::rex("Code and comments coming after a return() or stop()")
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
#     rex::rex("Code and comments coming after a return() or stop()"),
#     unreachable_code_linter()
#   )
# })
# nolint end: commented_code_linter.

# TODO(michaelchirico): This could also apply to cases without
#   explicit returns (where it can only apply to comments)
