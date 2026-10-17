test_that("on_exit_linter skips allowed usages", {
  linter <- on_exit_linter()

  # Single on.exit() call in a block
  expect_no_lint("f <- function() { on.exit(cleanup()) }", linter)
  expect_no_lint("on.exit(cleanup())", linter)

  # Subsequent on.exit() calls with explicit add = TRUE or add = FALSE
  expect_no_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        on.exit(cleanup2(), add = TRUE)
        on.exit(cleanup3(), add = FALSE)
      }
    "),
    linter
  )

  # Positional second argument (add)
  expect_no_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        on.exit(cleanup2(), TRUE)
        on.exit(cleanup3(), FALSE)
        on.exit(expr = cleanup4(), TRUE)
      }
    "),
    linter
  )

  # Clearing calls: on.exit() and on.exit(NULL)
  expect_no_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        on.exit()
      }
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        on.exit(NULL)
      }
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        on.exit(expr = NULL)
      }
    "),
    linter
  )

  # Handler registered after an intervening clearing call is fresh
  expect_no_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        on.exit()
        on.exit(cleanup2())
      }
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        on.exit(NULL)
        on.exit(cleanup2())
      }
    "),
    linter
  )

  # Separate blocks / nested functions / conditional branches
  expect_no_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        g <- function() {
          on.exit(cleanup2())
        }
      }
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      f <- function(x) {
        if (x) {
          on.exit(cleanup1())
        } else {
          on.exit(cleanup2())
        }
      }
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      f <- function(x) {
        if (x) on.exit(cleanup1())
        on.exit(cleanup2())
      }
    "),
    linter
  )
})

test_that("on_exit_linter blocks subsequent calls missing add", {
  linter <- on_exit_linter()
  lint_msg <- rex::rex(
    "Provide 'add = TRUE' to add an exit handler, or 'add = FALSE' to overwrite the previous on.exit() call."
  )

  expect_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        on.exit(cleanup2())
      }
    "),
    list(lint_msg, line_number = 3L, column_number = 3L),
    linter
  )

  # Passing after = without add = still overwrites
  expect_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        on.exit(cleanup2(), after = FALSE)
      }
    "),
    list(lint_msg, line_number = 3L, column_number = 3L),
    linter
  )

  # Named expr = without add =
  expect_lint(
    trim_some("
      f <- function() {
        on.exit(expr = cleanup1())
        on.exit(expr = cleanup2())
      }
    "),
    list(lint_msg, line_number = 3L, column_number = 3L),
    linter
  )

  # Semicolon-separated statements inside a block (e.g. dev.hold(); on.exit(dev.flush()))
  expect_lint(
    trim_some("
      f <- function() {
        on.exit(par(op))
        dev.hold(); on.exit(dev.flush())
      }
    "),
    list(lint_msg, line_number = 3L, column_number = 15L),
    linter
  )
  expect_lint(
    trim_some("
      f <- function() {
        x <- 1; on.exit(par(op))
        on.exit(dev.flush())
      }
    "),
    list(lint_msg, line_number = 3L, column_number = 3L),
    linter
  )

  # Top-level calls in a script
  expect_lint(
    trim_some("
      on.exit(cleanup1())
      on.exit(cleanup2())
    "),
    list(lint_msg, line_number = 2L, column_number = 1L),
    linter
  )
  expect_lint(
    "on.exit(cleanup1()); on.exit(cleanup2())",
    list(lint_msg, line_number = 1L, column_number = 22L),
    linter
  )

  # Namespace-qualified base::on.exit()
  expect_lint(
    trim_some("
      f <- function() {
        base::on.exit(cleanup1())
        base::on.exit(cleanup2())
      }
    "),
    list(lint_msg, line_number = 3L, column_number = 3L),
    linter
  )
})

test_that("lints vectorize", {
  lint_msg <- rex::rex(
    "Provide 'add = TRUE' to add an exit handler, or 'add = FALSE' to overwrite the previous on.exit() call."
  )

  expect_lint(
    trim_some("
      f <- function() {
        on.exit(cleanup1())
        on.exit(cleanup2())
        on.exit(cleanup3(), add = TRUE)
        on.exit(cleanup4())
      }
    "),
    list(
      list(lint_msg, line_number = 3L, column_number = 3L),
      list(lint_msg, line_number = 5L, column_number = 3L)
    ),
    on_exit_linter()
  )
})
