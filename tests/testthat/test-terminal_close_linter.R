test_that("terminal_close_linter skips allowed cases", {
  linter <- terminal_close_linter()

  lines <- trim_some("
    foo <- function(bar) {
      tmp <- tempfile()
      on.exit(close(tmp))
      writeLines(bar, tmp)
      return(invisible())
    }
  ")
  expect_lint(lines, NULL, linter)

  lines <- trim_some("
    foo <- function(bar) {
      close <- bar + 1
      return(close)
    }
  ")
  expect_lint(lines, NULL, linter)

  lines <- trim_some("
    foo <- function(bar) {
      close <- bar + 1
      close
    }
  ")
  expect_lint(lines, NULL, linter)
})

test_that("terminal_close_linter blocks simple cases", {
  linter <- terminal_close_linter()
  lint_msg <- rex::rex("Use on.exit(close(x)) to close connections")

  lines <- trim_some("
    foo <- function(bar) {
      tmp <- tempfile()
      writeLines(bar, tmp)
      return(close(tmp))
    }
  ")
  expect_lint(lines, lint_msg, linter)

  lines <- trim_some("
    foo <- function(bar) {
      tmp <- tempfile()
      writeLines(bar, tmp)
      close(tmp)
    }
  ")
  expect_lint(lines, lint_msg, linter)
})
