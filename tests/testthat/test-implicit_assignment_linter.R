test_that("implicit_assignment_linter skips allowed usages", {
  linter <- implicit_assignment_linter()

  expect_lint("x <- 1L", NULL, linter)
  expect_lint("1L -> x", NULL, linter)
  expect_lint("x <<- 1L", NULL, linter)
  expect_lint("1L ->> x", NULL, linter)
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

  expect_lint(
    trim_some("
      map(
        .x = 1:4,
        .f = ~ (x <- .x + 1)
      )"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      map(
        .x = 1:4,
        .f = ~ .x + 1 -> x
      )"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      map(
        .x = 1:4,
        .f = ~ {
          x <- .x + 1
          x
        }
      )"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      lapply(1:4, function(x) {
        x <- x + 1
        x
      })"),
    NULL,
    linter
  )

  skip_if_not_r_version("4.1.0")
  expect_lint(
    trim_some("
      map(1:4, \\(x) {
        x <- x + 1
        x
      })"),
    NULL,
    linter
  )
})

test_that("implicit_assignment_linter respects except argument", {
  expect_lint(
    "local({ a <- 1L })",
    NULL,
    implicit_assignment_linter(except = NULL)
  )

  expect_lint(
    "local({ a <- 1L })",
    NULL,
    implicit_assignment_linter(except = character(0L))
  )

  expect_lint(
    "local(a <- 1L)",
    rex::rex("Avoid implicit assignments in function calls."),
    implicit_assignment_linter(except = character(0L))
  )

  expect_lint(
    "local(a <- 1L)",
    rex::rex("Avoid implicit assignments in function calls."),
    implicit_assignment_linter(except = NULL)
  )

  expect_lint(
    "local(a <- 1L)",
    NULL,
    implicit_assignment_linter(except = "local")
  )
})

test_that("implicit_assignment_linter skips allowed usages with braces", {
  linter <- implicit_assignment_linter(except = character(0L))

  expect_lint(
    trim_some("
    foo({
      a <- 1L
    })
    "),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
    output <- capture.output({
      x <- f()
    })
    "),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
    quote({
      a <- 1L
    })
    "),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
    bquote({
      a <- 1L
    })
    "),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
    expression({
      a <- 1L
    })
    "),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
    local({
      a <- 1L
    })
    "),
    NULL,
    linter
  )
})

test_that("implicit_assignment_linter makes exceptions for functions that capture side-effects", {
  linter <- implicit_assignment_linter()

  expect_lint(
    trim_some("
    test_that('my test', {
      a <- 1L
      expect_equal(a, 1L)
    })"),
    NULL,
    linter
  )

  # rlang
  expect_lint("expr(a <- 1L)", NULL, linter)
  expect_lint("quo(a <- 1L)", NULL, linter)
  expect_lint("quos(a <- 1L)", NULL, linter)

  # withr
  expect_lint("with_options(list(digits = 3L), x <- getOption('digits'))", NULL, linter)
})

test_that("implicit_assignment_linter blocks disallowed usages in simple conditional statements", {
  lint_message <- rex::rex("Avoid implicit assignments in function calls.")
  linter <- implicit_assignment_linter()

  expect_lint("if (x <- 1L) TRUE", lint_message, linter)
  expect_lint("if (1L -> x) TRUE", lint_message, linter)
  expect_lint("if (x <<- 1L) TRUE", lint_message, linter)
  expect_lint("if (1L ->> x) TRUE", lint_message, linter)
  expect_lint("while (x <- 0L) FALSE", lint_message, linter)
  expect_lint("while (0L -> x) FALSE", lint_message, linter)
  expect_lint("for (x in y <- 1:10) print(x)", lint_message, linter)
  expect_lint("for (x in 1:10 -> y) print(x)", lint_message, linter)
})

test_that("implicit_assignment_linter blocks disallowed usages in nested conditional statements", {
  lint_message <- rex::rex("Avoid implicit assignments in function calls.")
  linter <- implicit_assignment_linter()

  expect_lint(
    trim_some("
    while (x <- 1L) {
      if (0L -> y) FALSE
    }"),
    list(
      list(message = lint_message, line_number = 1L, column_number = 10L),
      list(message = lint_message, line_number = 2L, column_number = 10L)
    ),
    linter
  )
  expect_lint(
    trim_some("
    for (x in y <- 1:10) {
      if (0L -> y) print(x)
    }"),
    list(
      list(message = lint_message, line_number = 1L, column_number = 13L),
      list(message = lint_message, line_number = 2L, column_number = 10L)
    ),
    linter
  )
})

test_that("implicit_assignment_linter blocks disallowed usages in function calls", {
  lint_message <- rex::rex("Avoid implicit assignments in function calls.")
  linter <- implicit_assignment_linter()

  expect_lint("mean(x <- 1:4)", lint_message, linter)
  expect_lint("mean(x <- (y <- 1:3) + 1L)", lint_message, linter)
  expect_lint("y <- median(x <- 1:4)", lint_message, linter)
  expect_lint("lapply(x, function(x) return(x <- x + 1))", lint_message, linter)
  expect_lint("map(x, function(x) return(x <- x + 1))", lint_message, linter)

  expect_lint("expect_warning(out <- f(-1))", lint_message, linter)
  expect_lint("expect_message(out <- f(-1))", lint_message, linter)
  expect_lint("expect_error(out <- f(-1))", lint_message, linter)
  expect_lint("expect_condition(out <- f(-1))", lint_message, linter)

  expect_lint(
    trim_some("
    foo <- function(x) {
      return(x <- x + 1)
    }"),
    lint_message,
    linter
  )
  expect_lint(
    trim_some("
    foo <- function(x) {
      if (x <- 1L) x <- 2L
      return(x <- x + 1)
    }"),
    list(
      list(message = lint_message, line_number = 2L, column_number = 9L),
      list(message = lint_message, line_number = 3L, column_number = 12L)
    ),
    linter
  )
})

test_that("implicit_assignment_linter works as expected with pipes and walrus operator", {
  linter <- implicit_assignment_linter()

  expect_lint("data %>% mutate(a := b)", NULL, linter)
  expect_lint("dt %>% .[, z := x + y]", NULL, linter)
  expect_lint("data %<>% mutate(a := b)", NULL, linter)

  expect_lint("DT[i, x := i]", NULL, linter)

  skip_if_not_r_version("4.1.0")

  expect_lint("data |> mutate(a := b)", NULL, linter)
})
