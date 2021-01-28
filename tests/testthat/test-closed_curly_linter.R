test_that("returns the correct linting", {
  closed_curly_message_regex <- rex(
    paste(
      "Closing curly-braces should always be on their own line,",
      "unless they are followed by an else."
    ))

  expect_lint("blah",
    NULL,
    closed_curly_linter())

  expect_lint("a <- function() {\n}",
    NULL,
    closed_curly_linter())

  expect_lint("a <- function() { 1 }",
    closed_curly_message_regex,
    closed_curly_linter())

  expect_lint("a <- function() { 1 }",
    closed_curly_message_regex,
    closed_curly_linter())

  expect_lint("a <- function() { 1 }",
    NULL,
    closed_curly_linter(allow_single_line = TRUE))

  expect_lint("a <- if(1) {\n 1} else {\n 2\n}",
    closed_curly_message_regex,
    closed_curly_linter())

  expect_lint("a <- if(1) {\n 1\n} else {\n 2}",
    closed_curly_message_regex,
    closed_curly_linter())

  expect_lint("a <- if(1) {\n 1} else {\n 2}",
    list(
      closed_curly_message_regex,
      closed_curly_message_regex
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
