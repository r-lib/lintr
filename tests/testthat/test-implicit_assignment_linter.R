# fuzzer disable: assignment
test_that("implicit_assignment_linter skips allowed usages", {
  linter <- implicit_assignment_linter()

  expect_no_lint("x <- 1L", linter)
  expect_no_lint("1L -> x", linter)
  expect_no_lint("x <<- 1L", linter)
  expect_no_lint("1L ->> x", linter)
  expect_no_lint("y <- if (is.null(x)) z else x", linter)
  expect_no_lint("for (x in 1:10) x <- x + 1", linter)

  expect_no_lint("abc <- mean(1:4)", linter)
  expect_no_lint("mean(1:4) -> abc", linter)

  expect_no_lint(
    trim_some("
    x <- 1:4
    mean(x)"),
    linter
  )

  expect_no_lint(
    trim_some("
    x <- 1L
    if (x) TRUE"),
    linter
  )

  expect_no_lint(
    trim_some("
    0L -> abc
    while (abc) {
      FALSE
    }"),
    linter
  )

  expect_no_lint(
    trim_some("
    if (x > 20L) {
      x <- x / 2.0
    }"),
    linter
  )

  expect_no_lint(
    trim_some("
    i <- 1
    while (i < 6L) {
      print(i)
      i <- i + 1
    }"),
    linter
  )

  expect_no_lint(
    trim_some("
    foo <- function(x) {
      x <- x + 1
      return(x)
    }"),
    linter
  )

  expect_no_lint(
    trim_some("
    f <- function() {
      p <- g()
      p <- if (is.null(p)) x else p
    }"),
    linter
  )

  expect_no_lint(
    trim_some("
      map(
        .x = 1:4,
        .f = ~ {
          x <- .x + 1
          x
        }
      )"),
    linter
  )

  expect_no_lint(
    trim_some("
      lapply(1:4, function(x) {
        x <- x + 1
        x
      })"),
    linter
  )

  expect_no_lint(
    trim_some("
      map(1:4, \\(x) {
        x <- x + 1
        x
      })"),
    linter
  )
})

test_that("implicit_assignment_linter respects except argument", {
  expect_no_lint(
    "local({ a <- 1L })",
    implicit_assignment_linter(except = NULL)
  )

  expect_no_lint(
    "local({ a <- 1L })",
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

  expect_no_lint(
    "local(a <- 1L)",
    implicit_assignment_linter(except = "local")
  )

  expect_error(
    implicit_assignment_linter(except = 1L),
    "`except` must be a character",
    fixed = TRUE
  )
})

test_that("implicit_assignment_linter skips allowed usages with braces", {
  linter <- implicit_assignment_linter(except = character(0L))

  expect_no_lint(
    trim_some("
    foo({
      a <- 1L
    })
    "),
    linter
  )
  expect_no_lint(
    trim_some("
    output <- capture.output({
      x <- f()
    })
    "),
    linter
  )
  expect_no_lint(
    trim_some("
    quote({
      a <- 1L
    })
    "),
    linter
  )
  expect_no_lint(
    trim_some("
    bquote({
      a <- 1L
    })
    "),
    linter
  )
  expect_no_lint(
    trim_some("
    expression({
      a <- 1L
    })
    "),
    linter
  )
  expect_no_lint(
    trim_some("
    local({
      a <- 1L
    })
    "),
    linter
  )
})

test_that("implicit_assignment_linter makes exceptions for functions that capture side-effects", {
  linter <- implicit_assignment_linter()

  expect_no_lint(
    trim_some("
    test_that('my test', {
      a <- 1L
      expect_equal(a, 1L)
    })"),
    linter
  )

  # rlang
  expect_no_lint("expr(a <- 1L)", linter)
  expect_no_lint("quo(a <- 1L)", linter)
  expect_no_lint("quos(a <- 1L)", linter)
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

  # adversarial commenting
  expect_lint(
    trim_some("
      while # comment
      (x <- 0L) FALSE

      while ( # comment
      x <- 0L) FALSE
    "),
    list(
      list(lint_message, line_number = 2L),
      list(lint_message, line_number = 5L)
    ),
    linter
  )
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
      list(message = lint_message, line_number = 1L, column_number = 8L),
      list(message = lint_message, line_number = 2L, column_number = 7L)
    ),
    linter
  )
  expect_lint(
    trim_some("
    for (x in y <- 1:10) {
      if (0L -> y) print(x)
    }"),
    list(
      list(message = lint_message, line_number = 1L, column_number = 11L),
      list(message = lint_message, line_number = 2L, column_number = 7L)
    ),
    linter
  )
})

test_that("implicit_assignment_linter blocks disallowed usages in function calls", {
  lint_message <- rex::rex("Avoid implicit assignments in function calls.")
  linter <- implicit_assignment_linter()

  expect_lint("mean(x <- 1:4)", lint_message, linter)
  expect_lint(
    "mean(x <- (y <- 1:3) + 1L)",
    list(list(column_number = 6L), list(column_number = 12L)),
    linter
  )
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
      list(message = lint_message, line_number = 2L, column_number = 7L),
      list(message = lint_message, line_number = 3L, column_number = 10L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      map(
        .x = 1:4,
        .f = ~ .x + 1 -> x
      )"),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      map(
        .x = 1:4,
        .f = ~ (x <- .x + 1)
      )"),
    lint_message,
    linter
  )


  expect_lint(
    "foo(a <- 1, b <- 2, c <- 3)",
    list(list(column_number = 5L), list(column_number = 13L), list(column_number = 21L)),
    linter
  )
})

test_that("implicit_assignment_linter works as expected with pipes and walrus operator", {
  linter <- implicit_assignment_linter()

  expect_no_lint("data %>% mutate(a := b)", linter)
  expect_no_lint("dt %>% .[, z := x + y]", linter)
  expect_no_lint("data %<>% mutate(a := b)", linter)
  expect_no_lint("data |> mutate(a := b)", linter)

  expect_no_lint("DT[i, x := i]", linter)
})

test_that("parenthetical assignments are caught", {
  expect_lint(
    "if (A && (B <- foo())) { }",
    rex::rex("Avoid implicit assignments in function calls."),
    implicit_assignment_linter()
  )
})

test_that("allow_lazy lets lazy assignments through", {
  linter <- implicit_assignment_linter(allow_lazy = TRUE)
  lint_message <- rex::rex("Avoid implicit assignments in function calls.")

  expect_no_lint("A && (B <- foo(A))", linter)
  # || also admits laziness
  expect_no_lint("A || (B <- foo(A))", linter)
  # & and |, however, do not
  expect_lint("A & (B <- foo(A))", lint_message, linter)
  expect_lint("A | (B <- foo(A))", lint_message, linter)
  expect_no_lint("A && foo(bar(idx <- baz()))", linter)
  # LHS _is_ linted
  expect_lint("(A <- foo()) && B", lint_message, linter)
  # however we skip on _any_ RHS (even if it's later an LHS)
  # test on all &&/|| combinations to stress test operator precedence
  expect_no_lint("A && (B <- foo(A)) && C", linter)
  expect_no_lint("A && (B <- foo(A)) || C", linter)
  expect_no_lint("A || (B <- foo(A)) && C", linter)
  expect_no_lint("A || (B <- foo(A)) || C", linter)
  # &&/|| elsewhere in the tree don't matter
  expect_lint(
    trim_some("
      A && B
      foo(C <- bar())
    "),
    lint_message,
    linter
  )
})

test_that("allow_scoped skips scoped assignments", {
  linter <- implicit_assignment_linter(allow_scoped = TRUE)
  lint_message <- rex::rex("Avoid implicit assignments in function calls.")

  expect_no_lint(
    trim_some("
      if (any(idx <- x < 0)) {
        stop('negative elements: ', toString(which(idx)))
      }
    "),
    linter
  )
  expect_lint(
    trim_some("
      if (any(idx <- x < 0)) {
        stop('negative elements: ', toString(which(idx)))
      }
      print(idx)
    "),
    lint_message,
    linter
  )

  expect_no_lint("if (a <- 1) print(a)", linter)

  # only applies to the branch condition itself -- within the branch, still lint
  expect_lint(
    trim_some("
      if (TRUE) {
        foo(idx <- bar())
      }
    "),
    lint_message,
    linter
  )

  expect_no_lint(
    trim_some("
      obj <- letters
      while ((n <- length(obj)) > 0) obj <- obj[-n]
    "),
    linter
  )
  expect_lint(
    trim_some("
      obj <- letters
      while ((n <- length(obj)) > 0) obj <- obj[-n]
      if (TRUE) {
        print(n)
      }
    "),
    lint_message,
    linter
  )

  # outside of branching, doesn't matter
  expect_lint("foo(idx <- bar()); baz()", lint_message, linter)
  expect_lint("foo(x, idx <- bar()); baz()", lint_message, linter)

  # adversarial comments
  expect_no_lint(
    trim_some("
      if # comment
      (any(idx <- x < 0)) {
        stop('negative elements: ', toString(which(idx)))
      }
    "),
    linter
  )
})

test_that("interaction of allow_lazy and allow_scoped", {
  linter <- implicit_assignment_linter(allow_scoped = TRUE, allow_lazy = TRUE)

  expect_no_lint(
    trim_some("
      if (any(idx <- foo()) && BB) {
        stop('Invalid foo() output: ', toString(idx))
      }
    "),
    linter
  )
  expect_lint(
    trim_some("
      if (any(idx <- foo()) && BB) {
        stop('Invalid foo() output: ', toString(idx))
      }
      print(format(idx))
    "),
    rex::rex("Avoid implicit assignments in function calls."),
    linter
  )
  expect_no_lint(
    trim_some("
      if (AA && any(idx <- foo())) {
        stop('Invalid foo() output: ', toString(idx))
      }
      print(format(idx)) # NB: bad code! idx may not exist.
    "),
    linter
  )
})

test_that("call-less '(' mentions avoiding implicit printing", {
  linter <- implicit_assignment_linter()
  implicit_msg <- rex::rex("Avoid implicit assignments in function calls.")
  print_msg <- rex::rex("Call print() explicitly instead of relying on implicit printing behavior via '('.")

  expect_lint("(a <- foo())", print_msg, linter)

  # only for top-level assignments; withAutoprint() ignored
  expect_lint("for (xi in x) (z <- foo(xi))", implicit_msg, linter)

  # mixed messages
  expect_lint(
    trim_some("
      (a <- foo())
      bar(z <- baz(a))
    "),
    list(print_msg, implicit_msg),
    linter
  )
})

test_that("allow_paren_print allows `(` for auto printing", {
  lint_message <- rex::rex("Avoid implicit assignments in function calls.")
  linter <- implicit_assignment_linter(allow_paren_print = TRUE)
  expect_no_lint("(a <- foo())", linter)

  # Doesn't effect other cases
  lint_message <- rex::rex("Avoid implicit assignments in function calls.")
  expect_lint("if (x <- 1L) TRUE", lint_message, linter)
  expect_lint("while (x <- 0L) FALSE", lint_message, linter)
  expect_lint("for (x in 1:10 -> y) print(x)", lint_message, linter)
  expect_lint("mean(x <- 1:4)", lint_message, linter)

  # default remains as is
  print_msg <- rex::rex("Call print() explicitly instead of relying on implicit printing behavior via '('.")
  expect_lint("(a <- foo())", print_msg, implicit_assignment_linter())
})
# fuzzer enable: assignment
