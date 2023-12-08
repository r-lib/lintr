test_that("function_return_linter skips allowed usages", {
  lines_simple <- trim_some("
    foo <- function(x) {
      x <- x + 1
      return(x)
    }
  ")
  expect_lint(lines_simple, NULL, function_return_linter())

  # arguably an expression as complicated as this should also be assigned,
  #   but for now that's out of the scope of this linter
  lines_subassignment <- trim_some("
    foo <- function(x) {
      return(x[, {
        col <- col + 1
        .(grp, col)
      }])
    }
  ")
  expect_lint(lines_subassignment, NULL, function_return_linter())
})

test_that("function_return_linter blocks simple disallowed usages", {
  linter <- function_return_linter()
  lint_msg <- rex::rex("Move the assignment outside of the return() clause")

  expect_lint(
    trim_some("
      foo <- function(x) {
        return(x <- x + 1)
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        return(x <<- x + 1)
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        return(x + 1 ->> x)
      }
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        return(x + 1 -> x)
      }
    "),
    lint_msg,
    linter
  )

  side_effect_lines <- expect_lint(
    trim_some("
      e <- new.env()
      foo <- function(x) {
        return(e$val <- x + 1)
      }
    "),
    lint_msg,
    linter
  )
})

test_that("lints vectorize", {
  linter <- function_return_linter()
  lint_msg <- rex::rex("Move the assignment outside of the return() clause")

  expect_lint(
    trim_some("{
      function() {
        return(x <- 1)
      }
      function() {
        return(y <- 2)
      }
    }"),
    list(
      list(lint_msg, line_number = 3L),
      list(lint_msg, line_number = 6L)
    ),
    function_return_linter()
  )
})
