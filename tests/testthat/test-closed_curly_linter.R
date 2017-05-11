context("closed_curly_linter")

test_that("returns the correct linting", {
  msg <- rex("Closing curly-braces should always be on their own line, unless it's followed by an else.")
  linter <- closed_curly_linter()
  expect_is(linter, "linter")

  expect_lint("blah", NULL, linter)

  expect_lint("a <- function() {\n}", NULL, linter)

  expect_lint("a <- function() { 1 }", msg, linter)

  expect_lint("a <- function() { 1 }", msg, linter)

  expect_lint("a <- function() { 1 }",
              NULL,
              closed_curly_linter(allow_single_line = TRUE))

  expect_lint("a <- if(1) {\n 1} else {\n 2\n}", msg, linter)

  expect_lint("a <- if(1) {\n 1\n} else {\n 2}", msg, linter)

  expect_lint("a <- if(1) {\n 1} else {\n 2}", list(msg, msg), linter)

  expect_lint("eval(bquote({...}))", NULL, linter)

  expect_lint("fun({\n  statements\n}, param)", NULL, linter)

  expect_lint("out <- lapply(stuff, function(i) {\n  do_something(i)\n}) %>% unlist",
              NULL,
              linter)
})
