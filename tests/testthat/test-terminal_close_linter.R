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

  expect_lint(
    trim_some("
      foo <- function(bar) {
        tmp <- tempfile()
        writeLines(bar, tmp)
        return(close(tmp))
      }
    "),
    list(lint_msg, line_number = 4L, column_number = 3L),
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(bar) {
        tmp <- tempfile()
        writeLines(bar, tmp)
        close(tmp)
      }
    "),
    list(lint_msg, line_number = 4L, column_number = 3L),
    linter
  )

  # When multiple terminations happen, only lint the one
  expect_lint(
    trim_some("
      foo <- function(bar) {
        tmp1 <- tempfile()
        tmp2 <- tempfile()
        writeLines(bar, tmp1)
        writeLines(bar, tmp2)
        close(tmp1)
        close(tmp2)
      }
    "),
    list(lint_msg, line_number = 7L, column_number = 3L),
    linter
  )
})
