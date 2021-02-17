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

test_that("used symbols are detected correctly", {
  # From #666
  expect_lint(
    trim_some("
      foo <- data.frame(0)
      foo$bar <- 1
      zero <- function() {
        file.info(\"/dev/null\")$size
      }
      message(zero())
    "),
    NULL,
    object_usage_linter()
  )

  expect_lint(
    trim_some("
      foo$bar <- 1
      zero <- function() {
        foo
      }
      message(zero())
    "),
    list("foo"),
    object_usage_linter()
  )

  # Also test deeper nesting
  expect_lint(
    trim_some("
      foo <- list(0)
      foo$bar$baz$goo <- 1
      zero <- function() {
        file.info(\"/dev/null\")$size
        foo$bar
        foo$bar$baz
        foo$bar$baz$goo
      }
      message(zero())
    "),
    NULL,
    object_usage_linter()
  )

  # Test alternative assignment and access methods
  expect_lint(
    trim_some("
      foo <- list(0)
      foo[['bar']][['baz']][['goo']] <- 1
      zero <- function() {
        file.info(\"/dev/null\")$size
        foo$bar
        foo$bar$baz
        foo$bar$baz$goo
        foo[[\"bar\"]]
        foo[[c(\"bar\", \"baz\")]]
        foo[[\"bar\"]]$baz$goo
      }
      message(zero())
    "),
    NULL,
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
    ),
    object_usage_linter()
  )

  # Linted symbol is not on the first line of the usage warning
  expect_lint(
    trim_some("
      foo <- function(x) {
        with(
          x,
          unknown_symbol
        )
      }
    "),
    list(message = "unknown_symbol", line_number = 4L, column_number = 5L),
    object_usage_linter()
  )

  # Kill regex match to enforce fallback to line 1 column 1 of the warning
  expect_lint(
    trim_some("
      foo <- function(x) {
        with(
          x,
          `\u2019regex_kill`
        )
      }
    "),
    list(line_number = 2L, column_number = 1L),
    object_usage_linter()
  )
})

test_that("global variable detection works", {
  old_globals <- utils::globalVariables(package = globalenv())
  utils::globalVariables("global_function", package = globalenv())
  on.exit(utils::globalVariables(old_globals, package = globalenv(), add = FALSE))

  expect_lint(
    trim_some("
      foo <- function() {
        if (global_function()) NULL

        if (global_function()) {
          NULL
        }
      }
    "),
    NULL,
    object_usage_linter()
  )
})

test_that("package detection works", {
  expect_length(
    lint_package("dummy_packages/package", linters = object_usage_linter(), parse_settings = FALSE),
    9L
  )
})

test_that("robust against errors", {
  expect_lint(
    "assign(\"x\", unknown_function)",
    NULL,
    object_usage_linter()
  )
})
