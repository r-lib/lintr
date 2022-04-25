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
# TODO(michaelchirico): similarly, return(x); x+1 should also lint, even though
#   the styler won't allow this in our current setup.
# TODO(michaelchirico): again similarly, this could also apply to cases without
#   explicit returns (where it can only apply to comments)
