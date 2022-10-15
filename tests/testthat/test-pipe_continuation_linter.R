test_that("pipe-continuation correctly handles stand-alone expressions", {
  linter <- pipe_continuation_linter()
  lint_msg <- rex::rex("`%>%` should always have a space before it and a new line after it,")

  # Expressions without pipes are ignored
  expect_lint("blah", NULL, linter)

  # Pipe expressions on a single line are ignored
  expect_lint("foo %>% bar() %>% baz()", NULL, linter)

  # Pipe expressions spanning multiple lines with each expression on a line are ignored
  expect_lint(
    trim_some("
      foo %>%
        bar() %>%
        baz()
    "),
    NULL,
    linter
  )

  # Pipe expressions with multiple expression on a line are linted
  expect_lint(
    trim_some("
      foo %>% bar() %>%
        baz()
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      foo %>% bar() %>% baz() %>%
       qux()
    "),
    lint_msg,
    linter
  )
})

test_that("pipe-continuation linter correctly handles nesting", {
  linter <- pipe_continuation_linter()
  lint_msg <- rex::rex("`%>%` should always have a space before it and a new line after it,")

  expect_lint(
    trim_some("
      my_fun <- function(){
        a %>% b() %>%
          c()
      }
    "),
    list(list(message = lint_msg, line_number = 2L)),
    linter
  )

  expect_lint(
    trim_some("
      my_fun <- function(){
        a %>%
          b() %>% c()
      }
    "),
    list(list(message = lint_msg, line_number = 3L)),
    linter
  )

  # but no lints here
  expect_lint(
    trim_some("
      1:4 %>% {
       (.) %>% sum()
      }
    "),
    NULL,
    linter
  )
})

test_that("pipe-continuation linter handles native pipe", {
  skip_if_not_r_version("4.1.0")

  linter <- pipe_continuation_linter()
  lint_msg_native <- rex::rex("`|>` should always have a space before it and a new line after it,")
  lint_msg_magrittr <- rex::rex("`%>%` should always have a space before it and a new line after it,")

  expect_lint("foo |> bar() |> baz()", NULL, linter)
  expect_lint(
    trim_some("
      foo |>
        bar() |>
        baz()
    "),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
      foo |> bar() |>
        baz()
    "),
    lint_msg_native,
    linter
  )
  # mixing pipes
  expect_lint(
    trim_some("
      foo %>% bar() |>
        baz()
    "),
    lint_msg_native,
    linter
  )
  expect_lint(
    trim_some("
      foo |> bar() %>%
        baz()
    "),
    lint_msg_magrittr,
    linter
  )
  expect_lint(
    trim_some("
      list(
        foo |> bar() |>
          baz(),
        foo %>% bar() %>%
          baz()
      )
    "),
    list(
      lint_msg_native,
      lint_msg_magrittr
    ),
    linter
  )
})

local({
  linter <- pipe_continuation_linter()
  valid_code <- c(
    # all on one line
    trim_some("
      my_fun <- function() {
        a %>% b()
      }
    "),
    trim_some("
      my_fun <- function() {
        a %>% b() %>% c()
      }
    "),
    trim_some("
      with(
        diamonds,
        x %>% head(10) %>% tail(5)
      )
    "),
    trim_some("
      test_that('blah', {
        test_data <- diamonds %>% head(10) %>% tail(5)
      })
    "),

    # two different single-line pipelines
    trim_some("
      {
        x <- a %>% b %>% c
        y <- c %>% b %>% a
      }
    "),

    # at most one pipe-character per line
    trim_some("
      my_fun <- function() {
        a %>%
          b() %>%
          c()
      }
    ")
  )
  patrick::with_parameters_test_that(
    "valid nesting is handled",
    {
      expect_lint(code_string, NULL, linter)
    },
    .test_name = valid_code,
    code_string = valid_code
  )
})
