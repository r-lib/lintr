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
  expect_no_lint(lines, linter)

  lines <- trim_some("
    foo <- function(bar) {
      close <- bar + 1
      return(close)
    }
  ")
  expect_no_lint(lines, linter)

  lines <- trim_some("
    foo <- \\(bar) {
      close <- bar + 1
      return(close)
    }
  ")
  expect_no_lint(lines, linter)

  lines <- trim_some("
    foo <- function(bar) {
      close <- bar + 1
      close
    }
  ")
  expect_no_lint(lines, linter)
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

test_that("lints vectorize", {
  skip_if_not_r_version("4.1.0")

  expect_lint(
    trim_some("{
      foo <- function() {
        tmp <- file(tempfile())
        writeLines(letters, tmp)
        close(tmp)
      }
      bar <- \\() {
        tmp <- file(tempfile())
        writeLines(letters, tmp)
        close(tmp)
      }
    }"),
    list(
      list("close connections", line_number = 5L),
      list("close connections", line_number = 10L)
    ),
    terminal_close_linter()
  )
})
