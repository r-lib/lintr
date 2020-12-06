context("closed_curly_linter")
test_that("returns the correct linting", {

  expect_lint("blah",
    NULL,
    closed_curly_linter())

  expect_lint("a <- function() {\n}",
    NULL,
    closed_curly_linter())

  expect_lint("a <- function() { 1 }",
    rex("Closing curly-braces should always be on their own line, unless it's followed by an else."),
    closed_curly_linter())

  expect_lint("a <- function() { 1 }",
    rex("Closing curly-braces should always be on their own line, unless it's followed by an else."),
    closed_curly_linter())

  expect_lint("a <- function() { 1 }",
              NULL,
              closed_curly_linter(allow_single_line = TRUE))

  expect_lint("a <- if(1) {\n 1} else {\n 2\n}",
    rex("Closing curly-braces should always be on their own line, unless it's followed by an else."),
    closed_curly_linter())

  expect_lint("a <- if(1) {\n 1\n} else {\n 2}",
    rex("Closing curly-braces should always be on their own line, unless it's followed by an else."),
    closed_curly_linter())

  expect_lint("a <- if(1) {\n 1} else {\n 2}",
    list(
      rex("Closing curly-braces should always be on their own line, unless it's followed by an else."),
      rex("Closing curly-braces should always be on their own line, unless it's followed by an else.")
      ),
    closed_curly_linter())

  expect_lint("eval(bquote({...}))",
              NULL,
              closed_curly_linter())

  expect_lint("fun({\n  statements\n}, param)",
              NULL,
              closed_curly_linter())

  expect_lint("out <- lapply(stuff, function(i) {\n  do_something(i)\n}) %>% unlist",
              NULL,
              closed_curly_linter())

  expect_lint("{{x}}", NULL, closed_curly_linter())
})
