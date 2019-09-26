context("paren_brace_linter")

test_that("returns the correct linting", {
  msg <- rex("There should be a space between right parenthesis and an opening curly brace.")

  expect_lint("blah", NULL, paren_brace_linter)
  expect_lint("blah <- function() {}", NULL, paren_brace_linter)
  expect_lint("blah <- function() {\n}", NULL, paren_brace_linter)

  expect_lint(
    "blah <- function(){}",
    msg,
    paren_brace_linter
  )

  expect_lint(
    "\nblah <- function(){\n\n\n}",
    msg,
    paren_brace_linter
  )
})
