test_that("returns the correct linting", {
  linter <- object_usage_linter()

  expect_lint("blah", NULL, linter)

  expect_lint(
    trim_some("
      function() {
        a <- 1
        a
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      fun <- function(x) {
        fun(1)
      }
      fun2 <- function(x) {
        fun2(2)
      }
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      fun <- function() {
        a <- 1
      }
    "),
    rex("local variable", anything, "assigned but may not be used"),
    linter
  )

  expect_lint(
    trim_some("
      fun <- function() {
        a <- 1
        1
      }
    "),
    rex("local variable", anything, "assigned but may not be used"),
    linter
  )

  expect_lint(
    trim_some("
      fun <- function() {
        a <- 1
      }
    "),
    rex("local variable", anything, "assigned but may not be used"),
    linter
  )

  expect_lint(
    trim_some("
      fun <- function() {
        a2 <- 1
        a3
      }
    "),
    list(
      rex("local variable", anything, "assigned but may not be used"),
      rex("no visible binding for global variable ", anything)
    ),
    linter
  )

  expect_lint(
    trim_some("
      fun <- function() {
        fnu(1)
      }
    "),
    rex("no visible global function definition for ", anything),
    linter
  )

  expect_lint(
    trim_some("
      fun <- function(x) {
        n(1)
      }
    "),
    rex("no visible global function definition for ", anything),
    linter
  )
})

test_that("replace_functions_stripped", {
  expect_lint(
    trim_some("
      fun <- function(x) {
        n(x) = 1
      }
    "),
    rex("no visible global function definition for ", anything),
    object_usage_linter()
  )

  expect_lint(
    trim_some("
      fun <- function(x) {
        n(x) <- 1
      }
    "),
    rex("no visible global function definition for ", anything),
    object_usage_linter()
  )
})

test_that("eval errors are ignored", {
  expect_lint(
    trim_some("
    setMethod(\"[[<-\", c(\"stampedEnv\", \"character\", \"missing\"),
      function(x) {
        x
      })
    "),
    NULL,
    object_usage_linter()
  )
})

test_that("calls with top level function definitions are ignored", {
  expect_lint(
    'tryCatch("foo", error = function(e) e)',
    NULL,
    object_usage_linter()
  )
})

test_that("object-usage line-numbers are relative to start-of-file", {
  expect_lint(
    trim_some("
      a <- function(y) {
        y ** 2
      }
      b <- function() {
        x
      }
    "),
    list(line_number = 5L),
    object_usage_linter()
  )
})

test_that("object_usage_linter finds lints spanning multiple lines", {
  # Regression test for #507
  expect_lint(
    trim_some("
      foo <- function() {
        if (unknown_function()) NULL

        if (unknown_function()) {
          NULL
        }
      }
    "),
    list(
      list(message = "unknown_function", line_number = 2L),
      list(message = "unknown_function", line_number = 4L)
    )
  )
})