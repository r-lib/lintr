test_that("brace_linter lints closed braces correctly", {
  closed_curly_msg <- rex::rex(paste(
    "Closing curly-braces should always be on their own line,",
    "unless they are followed by an else."
  ))

  linter <- brace_linter()
  expect_lint("blah", NULL, linter)
  expect_lint("a <- function() {\n}", NULL, linter)

  expect_lint("a <- function() { 1 }", closed_curly_msg, linter)
  # allowed by allow_single_line
  expect_lint("a <- function() { 1 }", NULL, brace_linter(allow_single_line = TRUE))

  expect_lint(
    trim_some("
      a <- if(1) {
        1} else {
        2
      }
    "),
    closed_curly_msg,
    linter
  )
  expect_lint(
    trim_some("
      a <- if(1) {
        1
      } else {
        2}
    "),
    closed_curly_msg,
    linter
  )

  expect_lint(
    trim_some("
      a <- if(1) {
        1} else {
        2}
    "),
    list(
      closed_curly_msg,
      closed_curly_msg
    ),
    linter
  )

  # }) is allowed
  expect_lint("eval(bquote({...}))", NULL, linter)

  # }, is allowed
  expect_lint(
    trim_some("
      fun({
        statements
      }, param)"),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
      fun(function(a) {
        statements
      }, param)"),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      out <- lapply(stuff, function(i) {
        do_something(i)
      }) %>% unlist
    "),
    NULL,
    linter
  )

  # }} is allowed
  expect_lint("{{ x }}", NULL, linter)
})
