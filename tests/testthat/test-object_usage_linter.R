test_that("returns the correct linting", {
  linter <- object_usage_linter()
  local_var_msg <- rex::rex("local variable", anything, "assigned but may not be used")

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
    local_var_msg,
    linter
  )

  expect_lint(
    trim_some("
      fun <- function() {
        a <- 1
        1
      }
    "),
    local_var_msg,
    linter
  )

  expect_lint(
    trim_some("
      fun <- function() {
        a <- 1
      }
    "),
    local_var_msg,
    linter
  )

  # same, using = for assignment
  expect_lint(
    trim_some("
      fun = function() {
        a = 1
      }
    "),
    local_var_msg,
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
      local_var_msg,
      rex::rex("no visible binding for global variable ", anything)
    ),
    linter
  )

  expect_lint(
    trim_some("
      fun <- function() {
        fnu(1)
      }
    "),
    rex::rex("no visible global function definition for ", anything),
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
    rex::rex("no visible global function definition for ", anything),
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
    local_var_msg,
    linter
  )

  expect_lint(
    trim_some("
      setMethod('plot', 'numeric', function() {
        a <- 1
        1
      })
    "),
    local_var_msg,
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
    rex::rex("no visible global function definition for ", anything),
    object_usage_linter()
  )

  expect_lint(
    trim_some("
      fun <- function(x) {
        `__lintr_obj`(x) <- 1
      }
    "),
    rex::rex("no visible global function definition for ", anything),
    object_usage_linter()
  )
})

test_that("eval errors are ignored", {
  expect_lint(
    trim_some('
    setMethod("[[<-", c("stampedEnv", "character", "missing"),
      function(x) {
        x
      })
    '),
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
      a <- \\(y) {
        y ** 2
      }
      b <- \\() {
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
    trim_some('
      foo <- data.frame(0)
      foo$bar <- 1
      zero <- function() {
        file.info("/dev/null")$size
      }
      message(zero())
    '),
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
    trim_some('
      foo <- list(0)
      foo$bar$baz$goo <- 1
      zero <- function() {
        file.info("/dev/null")$size
        foo$bar
        foo$bar$baz
        foo$bar$baz$goo
      }
      message(zero())
    '),
    NULL,
    object_usage_linter()
  )

  # Test alternative assignment and access methods
  expect_lint(
    trim_some('
      foo <- list(0)
      foo[["bar"]][["baz"]][["goo"]] <- 1
      zero <- function() {
        file.info("/dev/null")$size
        foo$bar
        foo$bar$baz
        foo$bar$baz$goo
        foo[["bar"]]
        foo[[c("bar", "baz")]]
        foo[["bar"]]$baz$goo
      }
      message(zero())
    '),
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
    object_usage_linter(skip_with = FALSE)
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
    object_usage_linter(skip_with = FALSE)
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
    10L
  )
})

test_that("robust against errors", {
  expect_lint(
    'assign("x", unknown_function)',
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

  # no need for namespace-qualification
  expect_lint(trim_some("
    glue <- glue::glue # imitate this being an @import
    fun <- function() {
      local_var <- 42
      glue('The answer is {local_var}.')
    }
  "), NULL, linter)

  # multiple variables in different interpolations
  expect_lint(trim_some("
    fun <- function() {
      local_key <- 'a'
      local_value <- 123
      glue::glue('Key-value pair: {local_key}={local_value}.')
    }
  "), NULL, linter)

  # multiple variables in single interpolation
  expect_lint(trim_some("
    fun <- function() {
      local_str1 <- 'a'
      local_str2 <- 'b'
      glue::glue('With our powers combined: {paste(local_str1, local_str2)}.')
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
  "), "local_var", object_usage_linter(interpret_extensions = NULL))

  # call in glue is caught
  expect_lint(
    trim_some("
      fun <- function() {
        local_call <- identity
        local_unused_call <- identity
        glue::glue('{local_call(1)}')
      }
    "),
    "local_unused_call",
    linter
  )

  # ditto infix operator
  expect_lint(trim_some("
    glue <- glue::glue # imitate this being an @import
    foo <- function() {
      `%++%` <- `+`
      glue('{x %++% y}')
    }
  "), NULL, linter)
})

test_that("errors/edge cases in glue syntax don't fail lint()", {
  linter <- object_usage_linter()

  # no lint & no error, despite glue error
  expect_warning(
    expect_lint(
      trim_some("
        fun <- function() {
          a <- 2
          a + 1
          glue::glue('The answer is {a')
        }
      "),
      NULL,
      linter
    ),
    "Evaluating glue expression.*failed: Expecting '\\}'.*Please ensure correct glue syntax"
  )

  # generates a lint because the "usage" inside glue() is not detected
  expect_warning(
    expect_lint(
      trim_some("
        fun <- function() {
          a <- 2
          glue::glue('The answer is {a')
        }
      "),
      "local variable 'a'",
      linter
    ),
    "Evaluating glue expression.*failed: Expecting '\\}'"
  )

  # complete {...}, but syntax error in code -> ignore
  expect_lint(
    trim_some("
      fun <- function() {
        a <- 2
        glue::glue('The answer is {a + }')
      }
    "),
    "local variable 'a'",
    linter
  )

  # empty glue expression {}
  expect_lint(
    trim_some("
      fun <- function() {
        a <- 2
        glue::glue('The answer is {}: {a}')
      }
    "),
    NULL,
    linter
  )

  # comment inside glue range (#1919)
  expect_lint(
    trim_some("
      fun <- function() {
        a <- 2
        glue::glue(
          'The answer is {}: {a}' # show the answer
        )
      }
    "),
    NULL,
    linter
  )
})

test_that("backtick'd names in glue are handled", {
  expect_lint(
    trim_some("
      fun <- function() {
        `w` <- 2
        x <- 3
        y <- -4
        `\\`y` <- 4
        z <- -5
        `z\\`` <- 5
        glue::glue('{w}{`x`}{y}{z}')
      }
    "),
    list(
      rex::rex("local variable '`y'"),
      rex::rex("local variable 'z`'")
    ),
    object_usage_linter()
  )
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
      line_number = 2L,
      column_number = 3L
    ),
    object_usage_linter()
  )
})

test_that("unknown infix operators give good lint metadata", {
  expect_lint(
    trim_some("
      foo <- function(x) {
        x %unknown-operator% 1
      }
    "),
    list(
      message = rex::rex("no visible global function definition for '%unknown-operator%'"),
      line_number = 2L, column_number = 5L
    ),
    object_usage_linter()
  )

  skip_if(any(c("package:rlang", "package:data.table") %in% search()))
  expect_lint(
    trim_some('
      foo <- function(x) {
        x[, "new_col" := 2L]
      }
    '),
    list(
      message = rex::rex("no visible global function definition for ':='"),
      line_number = 2L, column_number = 17L
    ),
    object_usage_linter()
  )
})

test_that("respects `skip_with` argument for `with()` expressions", {
  f <- withr::local_tempfile(
    lines = c(
      "test_fun <- function(df) {",
      "  with(df, first_var + second_var)",
      "}"
    )
  )

  expect_length(lint(f, object_usage_linter(skip_with = TRUE)), 0L)
  expect_length(lint(f, object_usage_linter(skip_with = FALSE)), 2L)
})

test_that("missing libraries don't cause issue", {
  expect_lint(
    trim_some("
      library(a.a.a.z.z.z)
      foo <- function() {
        a <- 1
        a
      }
    "),
    NULL,
    object_usage_linter()
  )
})

test_that("messages without a quoted name are caught", {
  # regression test for #1714
  expect_lint(
    trim_some("
      foo <- function() {
        a <- ...
        a
      }
    "),
    list(
      message = "... may be used in an incorrect context",
      line_number = 2L
    ),
    object_usage_linter()
  )
})

# See #1914
test_that("symbols in formulas aren't treated as 'undefined global'", {
  linter <- object_usage_linter()

  expect_lint(
    trim_some("
      foo <- function(x) {
        lm(
          y ~ z,
          data = x[!is.na(y)]
        )
      }
    "),
    list(
      message = "no visible binding for global variable 'y'",
      line_number = 4L,
      column_number = 21L
    ),
    linter
  )

  # neither on the RHS
  expect_lint(
    trim_some("
      foo <- function(x) {
        lm(
          z ~ y,
          data = x[!is.na(y)]
        )
      }
    "),
    list(
      message = "no visible binding for global variable 'y'",
      line_number = 4L,
      column_number = 21L
    ),
    linter
  )

  # nor in nested expressions
  expect_lint(
    trim_some("
      foo <- function(x) {
        lm(
          log(log(y)) ~ z,
          data = x[!is.na(y)]
        )
      }
    "),
    list(
      message = "no visible binding for global variable 'y'",
      line_number = 4L,
      column_number = 21L
    ),
    linter
  )

  # nor as a call
  # NB: I wanted this to be s(), as in mgcv::s(), but that
  #   doesn't work in this test suite because it resolves to
  #   rex::s() since we attach that in testthat.R
  expect_lint(
    trim_some("
      foo <- function(x) {
        lm(
          y(w) ~ z,
          data = x[!is.na(y)]
        )
      }
    "),
    list(
      message = "no visible binding for global variable 'y'",
      line_number = 4L,
      column_number = 21L
    ),
    linter
  )
})

test_that("NSE-ish symbols after $/@ are ignored as sources for lints", {
  linter <- object_usage_linter()
  lint_msg <- "no visible binding for global variable 'column'"

  expect_lint(
    trim_some("
      foo <- function(x) {
        ggplot2::ggplot(
          x[!is.na(x$column), ],
          ggplot2::aes(x = column, fill = factor(x$grp))
        )
      }
    "),
    list(lint_msg, line_number = 4L, column_number = 22L),
    linter
  )

  expect_lint(
    trim_some("
      foo <- function(x) {
        ggplot2::ggplot(
          x[!is.na(x@column), ],
          ggplot2::aes(x = column, fill = factor(x$grp))
        )
      }
    "),
    list(lint_msg, line_number = 4L, column_number = 22L),
    linter
  )
})

test_that("functional lambda definitions are also caught", {
  skip_if_not_r_version("4.1.0")

  expect_lint(
    trim_some("
      fun <- \\() {
        a <- 1
      }
    "),
    rex::rex("local variable", anything, "assigned but may not be used"),
    object_usage_linter()
  )
})

test_that("messages without location info are repaired", {
  linter <- object_usage_linter()
  global_function_msg <- rex::rex("no visible global function definition for", anything)
  global_variable_msg <- rex::rex("no visible binding for global variable", anything)
  local_variable_msg <- rex::rex("local variable", anything, "assigned but may not be used")

  # regression test for #1986
  expect_lint(
    "foo <- function() no_fun()",
    list(global_function_msg, line_number = 1L, column_number = 19L),
    linter
  )

  expect_lint(
    "foo <- function(a = no_fun()) a",
    list(global_function_msg, line_number = 1L, column_number = 21L),
    linter
  )

  expect_lint(
    "foo <- function() no_global",
    list(global_variable_msg, line_number = 1L, column_number = 19L),
    linter
  )

  expect_lint(
    "foo <- function() unused_local <- 42L",
    list(local_variable_msg, line_number = 1L, column_number = 19L),
    linter
  )

  # More complex case with two lints and missing location info
  expect_lint(
    trim_some("
      foo <- function() a <-
        bar()
    "),
    list(
      list(local_variable_msg, line_number = 1L, column_number = 19L),
      list(global_function_msg, line_number = 2L, column_number = 3L)
    ),
    linter
  )
})

test_that("globals in scripts are found regardless of assignment operator", {
  linter <- object_usage_linter()

  expect_lint(
    trim_some("
      library(dplyr)

      global_const_eq = 5
      global_const_la <- 6
      7 -> global_const_ra

      examplefunction <- function(df) {
        df %>%
          select(dist) %>%
          mutate(power = global_const_eq + global_const_ra + global_const_la)
      }
    "),
    NULL,
    linter
  )
})

test_that("dplyr's .env-specified objects are marked as 'used'", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("rlang")
  linter <- object_usage_linter()

  expect_lint(
    trim_some("
      foo <- function(df) {
        source <- 1
        target <- 2
        unused <- 3

        df %>%
          dplyr::mutate(
            from = rlang::.env$source,
            to = rlang::.env[['target']]
          )
      }
    "),
    rex::rex("local variable", anything, "unused"),
    linter
  )
})

test_that("interpret_glue is deprecated", {
  expect_warning(
    {
      linter_no <- object_usage_linter(interpret_glue = FALSE)
    },
    rex::rex("interpret_glue", anything, "deprecated")
  )
  expect_warning(
    {
      linter_yes <- object_usage_linter(interpret_glue = TRUE)
    },
    rex::rex("interpret_glue", anything, "deprecated")
  )

  code <- trim_some("
    fun <- function() {
      local_var <- 42
      glue::glue('The answer is {local_var}.')
    }
  ")
  expect_lint(code, "local_var", linter_no)
  expect_no_lint(code, linter_yes)
})
