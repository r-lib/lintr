context("pipe_continuation_linter")
test_that("returns the correct linting", {

  error <- rex("`%>%` should always have a space before it and a new line after it, unless the full pipeline fits on one line.")

  # Expressions without pipes are ignored
  expect_lint("blah", NULL, pipe_continuation_linter)

  # Pipe expressions on a single line are ignored
  expect_lint("foo %>% bar() %>% baz()", NULL, pipe_continuation_linter)

  # Pipe expressions spanning multiple lines with each expression on a line are ignored
  expect_lint("foo %>%\n  bar() %>%\n  baz()", NULL, pipe_continuation_linter)

  # Pipe expressions with multiple expression on a line are linted
  expect_lint("foo %>% bar() %>%\n  baz()", error,
    pipe_continuation_linter)

  expect_lint("foo %>% bar() %>% baz() %>%\n qux()",
    list(error, error),
    pipe_continuation_linter)
})
