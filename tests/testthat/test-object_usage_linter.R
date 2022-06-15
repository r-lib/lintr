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

  # same, using = for assignment
  expect_lint(
    trim_some("
      fun = function() {
        a = 1
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

  # earlier we used n(1) but this might conflict with dplyr::n(),
  #   so switch to use an obscure symbol
  expect_lint(
    trim_some("
      fun <- function(x) {
        `__lintr_obj`(1)
      }
    "),
    rex("no visible global function definition for ", anything),
    linter
  )

  # setMethod and assign also checked

  expect_lint(
    trim_some("
      assign('fun', function() {
        a <- 1
        1
      })
    "),
    rex("local variable", anything, "assigned but may not be used"),
    linter
  )

  expect_lint(
    trim_some("
      setMethod('plot', 'numeric', function() {
        a <- 1
        1
      })
    "),
    rex("local variable", anything, "assigned but may not be used"),
    linter
  )
})

test_that("replace_functions_stripped", {
  expect_lint(
    trim_some("
      fun <- function(x) {
        `__lintr_obj`(x) = 1
      }
    "),
    rex("no visible global function definition for ", anything),
    object_usage_linter()
  )

  expect_lint(
    trim_some("
      fun <- function(x) {
        `__lintr_obj`(x) <- 1
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



  # regression #1322
  expect_silent(expect_lint("assign('x', 42)", NULL, object_usage_linter()))
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

  # Even ugly names are found
  expect_lint(
    trim_some("
      foo <- function(x) {
        with(
          x,
          `\u2019regex_kill`
        )
      }
    "),
    list(line_number = 4L, column_number = 5L),
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
    lint_package(test_path("dummy_packages", "package"), linters = object_usage_linter(), parse_settings = FALSE),
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

test_that("interprets glue expressions", {
  linter <- object_usage_linter()

  expect_lint(trim_some("
    fun <- function() {
      local_var <- 42
      glue::glue('The answer is {local_var}.')
    }
  "), NULL, linter)

  # Check non-standard .open and .close
  expect_lint(trim_some("
    fun <- function() {
      local_var <- 42
      glue::glue('The answer is $[local_var].', .open = '$[', .close = ']')
    }
  "), NULL, linter)

  # Steer clear of custom .transformer and .envir constructs
  expect_lint(trim_some("
    fun <- function() {
      local_var <- 42
      glue::glue('The answer is {local_var}.', .transformer = glue::identity_transformer)
    }
  "), "local_var", linter)

  expect_lint(trim_some("
    fun <- function() {
      local_var <- 42
      e <- new.env()
      glue::glue('The answer is {local_var}.', .envir = e)
    }
  "), "local_var", linter)

  # unused is caught, glue-used is not
  expect_lint(trim_some("
    fun <- function() {
      local_var <- 42
      unused_var <- 3
      glue::glue('The answer is {local_var}.')
    }
  "), "unused_var", linter)

  # glue-only is caught with option off
  expect_lint(trim_some("
    fun <- function() {
      local_var <- 42
      glue::glue('The answer is {local_var}.')
    }
  "), "local_var", object_usage_linter(interpret_glue = FALSE))
})

# reported as #1088
test_that("definitions below top level are ignored (for now)", {
  expect_lint(
    trim_some("
      local({
        x <- 1
        f <- function() {
          x
        }
      })
    "),
    NULL,
    object_usage_linter()
  )
})

# reported as #1127
test_that("package imports are detected if present in file", {
  skip_if("package:xml2" %in% search())
  expect_lint(
    trim_some("
      dog <- function(url) {
        a <- read_xml(url)
        a
      }
    "),
    rex::rex("no visible global function definition for ", anything, "read_xml"),
    object_usage_linter()
  )

  expect_lint(
    trim_some("
      library(xml2)

      dog <- function(url) {
        a <- read_xml(url)
        a
      }
    "),
    NULL,
    object_usage_linter()
  )
})

test_that("fallback works", {
  expect_lint(
    trim_some("
      f <- function() {
        `non_existing_assign<-`(1, 2)
      }
    "),
    list(
      message = rex::rex("no visible global function definition for ", anything, "non_existing_assign<-"),
      column_number = 6L
    ),
    object_usage_linter()
  )
})
