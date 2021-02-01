test_that("returns the correct linting", {
  msg <- rex("There should be a space between right parenthesis and an opening curly brace.")

  expect_lint("blah", NULL, paren_brace_linter)
  expect_lint("blah <- function() {}", NULL, paren_brace_linter)
  expect_lint("blah <- function() {\n}", NULL, paren_brace_linter)

  expect_lint(
    "blah <- function(){}",
    list(
      message = msg,
      column_number = 19L
    ),
    paren_brace_linter
  )

  expect_lint(
    "\nblah <- function(){\n\n\n}",
    list(
      message = msg,
      column_number = 19L
    ),
    paren_brace_linter
  )

  # paren_brace_linter should ignore strings and comments, as in regexes:
  expect_lint(
    "grepl('(iss){2}', 'Mississippi')",
    NULL,
    paren_brace_linter
  )
  expect_lint(
    "x <- 123 # dont flag (paren){brace} if inside a comment",
    NULL,
    paren_brace_linter
  )
  # paren-brace lint should not be thrown when the brace lies on subsequent line
  expect_lint(
    paste(
      "x <- function()",
      "               {2}",
      sep = "\n"
    ),
    NULL,
    paren_brace_linter
  )
})
