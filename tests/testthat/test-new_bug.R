###############################################################################

context("pipe continuation bug: issue #366")

###############################################################################

test_that("reprex for pipe-continuation bug", {

  msg <- paste0(
    "`%>%` should always have a space before it and a new line after it,",
    " unless the full pipeline fits on one line."
  )

  # pipe-continuation linter should behave correctly for pipes within
  # subexpressions

  valid_code <- c(
    # all on one line
    "my_fun <- function(){\n  a %>% b()\n}\n",
    "my_fun <- function(){\n  a %>% b() %>% c()\n}\n",
    "with(\n  diamonds,\n  x %>% head(10) %>% tail(5)\n)\n",
    "test_that('blah', {\n  test_data <- diamonds %>% head(10) %>% tail(5)\n})",

    # at most one pipe-character per line
    "my_fun <- function(){\n  a %>%\n    b() %>%\n    c()\n}\n"
  )

  for (code_string in valid_code) {
    expect_lint(
      code_string,
      NULL,
      pipe_continuation_linter
    )
  }

  expect_lint(
    "my_fun <- function(){\n  a %>% b() %>%\n    c()\n}\n",
    list(
      list(message=msg, line_number=2L)
    ),
    pipe_continuation_linter
  )

  expect_lint(
    "my_fun <- function(){\n  a %>%\n    b() %>% c()\n}\n",
    list(
      list(message=msg, line_number=3L)
    ),
    pipe_continuation_linter
  )
})
