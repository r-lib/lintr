context("pipe_continuation_linter")

test_that("returns the correct linting", {
  msg <- rex("`%>%` should always have a space before it and a new line after it, unless the full pipeline fits on one line.")
  linter <- pipe_continuation_linter()
  expect_is(linter, "linter")

  # Expressions without pipes are ignored
  expect_lint("blah", NULL, linter)

  # Pipe expressions on a single line are ignored
  expect_lint("foo %>% bar() %>% baz()", NULL, linter)

  # Pipe expressions spanning multiple lines with each expression on a line are ignored
  expect_lint("foo %>%\n  bar() %>%\n  baz()", NULL, linter)

  # Pipe expressions with multiple expression on a line are linted
  expect_lint("foo %>% bar() %>%\n  baz()", msg, linter)

  expect_lint("foo %>% bar() %>% baz() %>%\n qux()", list(msg, msg), linter)
})
