# nofuzz start
test_that("pipe-continuation correctly handles stand-alone expressions", {
  linter <- pipe_continuation_linter()
  lint_msg <- rex::rex("Put a space before `%>%` and a new line after it,")

  # Expressions without pipes are ignored
  expect_no_lint("blah", linter)

  # Pipe expressions on a single line are ignored
  expect_no_lint("foo %>% bar() %>% baz()", linter)

  # Pipe expressions spanning multiple lines with each expression on a line are ignored
  expect_no_lint(
    trim_some("
      foo %>%
        bar() %>%
        baz()
    "),
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
  lint_msg <- rex::rex("Put a space before `%>%` and a new line after it,")

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
  expect_no_lint(
    trim_some("
      1:4 %>% {
       (.) %>% sum()
      }
    "),
    linter
  )
})

test_that("pipe-continuation linter handles native pipe", {
  linter <- pipe_continuation_linter()
  lint_msg_native <- rex::rex("Put a space before `|>` and a new line after it,")
  lint_msg_magrittr <- rex::rex("Put a space before `%>%` and a new line after it,")

  expect_no_lint("foo |> bar() |> baz()", linter)
  expect_no_lint(
    trim_some("
      foo |>
        bar() |>
        baz()
    "),
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
  .cases <- tibble::tribble(
    ~code_string,            ~.test_name,
    trim_some("
      my_fun <- function() {
        a %>% b()
      }
    "),                      "two on one line",
    trim_some("
      my_fun <- function() {
        a %>% b() %>% c()
      }
    "),                      "three on one line",
    trim_some("
      with(
        diamonds,
        x %>% head(10) %>% tail(5)
      )
    "),                      "three inside with()",
    trim_some("
      test_that('blah', {
        test_data <- diamonds %>% head(10) %>% tail(5)
      })
    "),                      "three inside test_that()",
    trim_some("
      {
        x <- a %>% b %>% c
        y <- c %>% b %>% a
      }
    "),                      "two different single-line pipelines",
    trim_some("
      my_fun <- function() {
        a %>%
          b() %>%
          c()
      }
    "),                      "at most one pipe-character per line"
  )
  patrick::with_parameters_test_that(
    "valid nesting is handled",
    # nolint next: unnecessary_nesting_linter. TODO(#2334): Remove this nolint.
    {
      expect_no_lint(code_string, linter)
    },
    .cases = .cases
  )
})

local({
  linter <- pipe_continuation_linter()
  pipes <- pipes()
  cases <- expand.grid(pipe1 = pipes, pipe2 = pipes, stringsAsFactors = FALSE)
  cases <- within(cases, {
    .test_name <- sprintf("(%s, %s)", pipe1, pipe2)
  })
  patrick::with_parameters_test_that(
    "Various pipes are linted correctly",
    expect_lint(
      sprintf("a %s b() %s\n  c()", pipe1, pipe2),
      rex::rex(sprintf("Put a space before `%s` and a new line after it", pipe2)),
      linter
    ),
    .cases = cases
  )
})
# nofuzz end
