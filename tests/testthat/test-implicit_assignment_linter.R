test_that("implicit_assignment_linter skips allowed usages", {
  linter <- implicit_assignment_linter()

  expect_lint("x <- 1L", NULL, linter)
  expect_lint("1L -> x", NULL, linter)

  expect_lint("x <- mean(1:4)", NULL, linter)

  expect_lint(
    trim_some("
    x <- 1:4
    mean(x)"),
    NULL,
    linter
  )


  expect_lint(
    trim_some("
    x <- 1L
    if (x) TRUE"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
    0L -> abc
    while (abc) {
      FALSE
    }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
    foo <- function(x) {
    x <- x + 1
      return(x)
    }"),
    NULL,
    linter
  )
})

test_that("implicit_assignment_linter blocks disallowed usages", {
  lint_message <- rex::rex("Avoid implicit assignments in function calls.")
  linter <- implicit_assignment_linter()

  expect_lint("if (x <- 1L) TRUE", lint_message, linter)
  expect_lint("if (1L -> x) TRUE", lint_message, linter)
  expect_lint("while (x <- 0L) FALSE", lint_message, linter)
  expect_lint("while (0L -> x) FALSE", lint_message, linter)

  expect_lint("mean(x <- 1:4)", lint_message, linter)
  expect_lint(
    trim_some("
    foo <- function(x) {
      return(x <- x + 1)
    }"),
    lint_message,
    linter
  )
})
