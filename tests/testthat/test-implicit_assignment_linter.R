test_that("implicit_assignment_linter skips allowed usages", {
  linter <- implicit_assignment_linter()

  expect_lint("x <- 1L", NULL, linter)
  expect_lint("1L -> x", NULL, linter)
  expect_lint("y <- if (is.null(x)) z else x", NULL, linter)
  expect_lint("for (x in 1:10) x <- x + 1", NULL, linter)

  expect_lint("abc <- mean(1:4)", NULL, linter)
  expect_lint("mean(1:4) -> abc", NULL, linter)

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
    if (x > 20L) {
      x <- x / 2.0
    }"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
    i <- 1
    while (i < 6L) {
      print(i)
      i <- i + 1
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

  expect_lint(
    trim_some("
    f <- function() {
      p <- g()
      p <- if (is.null(p)) x else p
    }"),
    NULL,
    linter
  )
})

test_that("implicit_assignment_linter makes exceptions for functions that capture side-effects", {
  linter <- implicit_assignment_linter()

  # base
  expect_lint("output <- capture.output(x <- f())", NULL, linter)
  expect_lint("quote(a <- 1L)", NULL, linter)
  expect_lint("bquote(a <- 1L)", NULL, linter)
  expect_lint("expression(a <- 1L)", NULL, linter)
  expect_lint("local({ a <- 1L })", NULL, linter)

  # rlang
  expect_lint("expr(a <- 1L)", NULL, linter)
  expect_lint("quo(a <- 1L)", NULL, linter)
  expect_lint("quos(a <- 1L)", NULL, linter)

  # withr
  expect_lint("with_options(list(digits = 3L), x <- getOption('digits'))", NULL, linter)

  # testthat
  expect_lint("expect_warning(out <- f(-1))", NULL, linter)
  expect_lint("expect_message(out <- f(-1))", NULL, linter)
  expect_lint("expect_error(out <- f(-1))", NULL, linter)
  expect_lint("expect_condition(out <- f(-1))", NULL, linter)
  expect_lint("expect_no_warning(out <- f(-1))", NULL, linter)
  expect_lint("expect_no_message(out <- f(-1))", NULL, linter)
  expect_lint("expect_no_error(out <- f(-1))", NULL, linter)
  expect_lint("expect_no_condition(out <- f(-1))", NULL, linter)
  expect_lint("expect_invisible(out <- f(-1))", NULL, linter)
  expect_lint("expect_visible(out <- f(-1))", NULL, linter)
  expect_lint("expect_silent(out <- f(-1))", NULL, linter)
  expect_lint("expect_output(out <- f(-1))", NULL, linter)
  expect_lint(
    trim_some("
    test_that('my test', {
      a <- 1L
      expect_equal(a, 1L)
    })"),
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
  expect_lint("for (x in y <- 1:10) print(x)", lint_message, linter)
  expect_lint("for (x in 1:10 -> y) print(x)", lint_message, linter)

  expect_lint("mean(x <- 1:4)", lint_message, linter)
  expect_lint("y <- median(x <- 1:4)", lint_message, linter)
  expect_lint("lapply(x, function(x) return(x <- x + 1))", lint_message, linter)
  expect_lint("map(x, function(x) return(x <- x + 1))", lint_message, linter)
  expect_lint(
    trim_some("
    foo <- function(x) {
      return(x <- x + 1)
    }"),
    lint_message,
    linter
  )
})
