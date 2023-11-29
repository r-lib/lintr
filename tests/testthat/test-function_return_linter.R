test_that("function_return_linter skips allowed usages", {
  linter <- function_return_linter()

  lines_simple <- function_body(signature = "x", "
    x <- x + 1
    return(x)
  ")
  expect_lint(lines_simple, NULL, linter)

  # arguably an expression as complicated as this should also be assigned,
  #   but for now that's out of the scope of this linter
  lines_subassignment <- function_body(signature = "x", "
    return(x[, {
      col <- col + 1
      .(grp, col)
    }])
  ")
  expect_lint(lines_subassignment, NULL, linter)
})

test_that("function_return_linter blocks simple disallowed usages", {
  linter <- function_return_linter()
  lint_msg <- rex::rex("Move the assignment outside of the return() clause")

  expect_lint(function_body("return(x <- x + 1)", signature = "x"), lint_msg, linter)

  expect_lint(function_body("return(x <<- x + 1)", signature = "x"), lint_msg, linter)

  expect_lint(function_body("return(x + 1 ->> x)", signature = "x"), lint_msg, linter)

  expect_lint(function_body("return(x + 1 -> x)", signature = "x"), lint_msg, linter)

  expect_lint(
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
