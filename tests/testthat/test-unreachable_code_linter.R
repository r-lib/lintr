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
  msg <- rex::rex("Remove code and comments coming after return() or stop()")

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
  msg <- rex::rex("Remove code and comments coming after `next` or `break`")

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
      message = rex::rex("Remove code and comments coming after return() or stop()")
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
    rex::rex("Remove code and comments coming after return() or stop()"),
    unreachable_code_linter()
  )
})

test_that("unreachable_code_linter finds expressions in the same line", {
  msg <- rex::rex("Remove code and comments coming after return() or stop()")
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
  msg <- rex::rex("Remove code and comments coming after return() or stop()")
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
    rex::rex("Remove code and comments coming after return() or stop()"),
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
    rex::rex("Remove code and comments coming after return() or stop()"),
    unreachable_code_linter()
  )
})

test_that("unreachable_code_linter ignores code after foo$stop(), which might be stopping a subprocess, for example", {
  linter <- unreachable_code_linter()

  expect_lint(
    trim_some("
      foo <- function(x) {
        bar <- get_process()
        bar$stop()
        TRUE
      }
    "),
    NULL,
    linter
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
    linter
  )
})

test_that("unreachable_code_linter ignores terminal nolint end comments", {
  linter <- unreachable_code_linter()

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
    list(linter, one_linter = assignment_linter())
  )

  expect_lint(
    trim_some("
      foo <- function() {
        do_something
        # TestNoLintStart: one_linter.
        a = 42
        next
        # TestNoLintEnd
      }
    "),
    NULL,
    linter
  )
})

test_that("unreachable_code_linter identifies unreachable code in conditional loops", {
  linter <- unreachable_code_linter()
  msg <- rex::rex("Remove code inside a conditional loop with a deterministically false condition.")

  lines <- trim_some("
    foo <- function(bar) {
      if (FALSE) {
        x <- 3
      }
      x + 3
    }
  ")

  expect_lint(lines, list(line_number = 2L, message = msg), linter)

  lines <- trim_some("
    foo <- function(bar) {
      if (FALSE) {
        # Unlinted comment
        x <- 3
      }
      x + 3
    }
  ")

  expect_lint(lines, list(line_number = 2L, message = msg), linter)

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

  expect_lint(lines, list(line_number = 4L, message = msg), linter)

  lines <- trim_some("
    foo <- function(bar) {
      while (FALSE) {
        x <- 3
      }
      x + 3
    }
  ")

  expect_lint(lines, list(line_number = 2L, message = msg), linter)

  lines <- trim_some("
    foo <- function(bar) {
      while (FALSE) {
        # Unlinted comment
        x <- 3
      }
      x + 3
    }
  ")

  expect_lint(lines, list(line_number = 2L, message = msg), linter)

  lines <- "while (FALSE) x <- 3"

  expect_lint(
    lines,
    list(line_number = 1L, ranges = list(c(1L, 20L)), message = msg),
    linter
  )

  lines <- "if (FALSE) x <- 3 # Test comment"

  expect_lint(
    lines,
    list(line_number = 1L, ranges = list(c(1L, 17L)), message = msg),
    linter
  )
})

test_that("unreachable_code_linter identifies unreachable code in conditional loops", {
  linter <- unreachable_code_linter()
  msg <- rex::rex("Remove code inside an else block after a deterministically true condition.")

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

  expect_lint(
    "if (TRUE) x <- 3 else if (bar) x + 3",
    list(line_number = 1L, ranges = list(c(23L, 36L)), message = msg),
    linter
  )
})

test_that("unreachable_code_linter identifies unreachable code in mixed conditional loops", {
  linter <- unreachable_code_linter()
  false_msg <- rex::rex("Remove code inside a conditional loop with a deterministically false condition.")
  true_msg <- rex::rex("Remove code inside an else block after a deterministically true condition.")

  expect_lint(
    trim_some("
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
    "),
    list(
      list(false_msg, line_number = 2L),
      list(false_msg, line_number = 5L),
      list(true_msg, line_number = 10L),
      list(rex::rex("Remove code and comments coming after return() or stop()."), line_number = 13L)
    ),
    linter
  )

  expect_lint(
    "if (FALSE) x <- 3 else if (TRUE) x + 3 else x + 4",
    list(
      list(false_msg, line_number = 1L, ranges = list(c(1L, 49L))),
      list(
        rex::rex("Remove code inside an else block after a deterministically true condition."),
        line_number = 1L,
        ranges = list(c(45L, 49L))
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
      message = rex::rex("Remove code and comments coming after return() or stop()")
    ),
    unreachable_code_linter()
  )
})

test_that("Do not lint inline else after stop", {

  expect_lint("if (x > 3L) stop() else x + 3", NULL, unreachable_code_linter())
})

test_that("Do not lint inline else after stop in inline function", {
  linter <- unreachable_code_linter()

  expect_lint("function(x) if (x > 3L) stop() else x + 3", NULL, linter)
  expect_lint("function(x) if (x > 3L) { stop() } else {x + 3}", NULL, linter)
})

test_that("Do not lint inline else after stop in inline lambda function", {
  skip_if_not_r_version("4.1.0")

  linter <- unreachable_code_linter()

  expect_lint("\\(x) if (x > 3L) stop() else x + 3", NULL, linter)
  expect_lint("\\(x){ if (x > 3L) stop() else x + 3 }", NULL, linter)
})

test_that("allow_comment_regex= works", {
  withr::local_options(c(lintr.exclude_end = "#\\s*TestNoLintEnd"))

  linter_covr <- unreachable_code_linter()
  linter_xxxx <- unreachable_code_linter(allow_comment_regex = "#.*xxxx")
  linter_x1x2 <- unreachable_code_linter(allow_comment_regex = c("#x", "#y"))

  expect_lint(
    trim_some("
      function() {
        return(1)
        # nocov end
      }
    "),
    NULL,
    linter_covr
  )

  expect_lint(
    trim_some("
      function() {
        return(1)
        # TestNoLintEnd
        # nocov end
      }
    "),
    NULL,
    linter_covr
  )

  expect_lint(
    trim_some("
      function() {
        return(1)
        # ABCDxxxx
      }
    "),
    NULL,
    linter_xxxx
  )

  expect_lint(
    trim_some("
      function() {
        return(1)
        # TestNoLintEnd
        # ABCDxxxx
      }
    "),
    NULL,
    linter_xxxx
  )

  expect_lint(
    trim_some("
      function() {
        return(1)
        #x
      }
    "),
    NULL,
    linter_x1x2
  )

  expect_lint(
    trim_some("
      function() {
        return(1)
        #xABC
        #yDEF
      }
    "),
    NULL,
    linter_x1x2
  )

  # might contain capture groups, #2678
  expect_lint(
    trim_some("
      function() {
        stop('a')
        # a
        # ab
      }
    "),
    NULL,
    unreachable_code_linter(allow_comment_regex = "#\\s*(a|ab|abc)")
  )
})

test_that("allow_comment_regex= obeys covr's custom exclusion when set", {
  withr::local_options(c(
    lintr.exclude_end = "#\\s*TestNoLintEnd",
    covr.exclude_end = "#\\s*TestNoCovEnd"
  ))

  linter_covr <- unreachable_code_linter()

  expect_lint(
    trim_some("
      function() {
        return(1)
        # TestNoCovEnd
      }
    "),
    NULL,
    linter_covr
  )

  expect_lint(
    trim_some("
      function() {
        return(1)
        # TestNoLintEnd
        # TestNoCovEnd
      }
    "),
    NULL,
    linter_covr
  )
})
