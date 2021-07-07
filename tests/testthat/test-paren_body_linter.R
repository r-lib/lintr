testthat::test_that("paren_body_linter returns correct lints", {
  linter <- paren_body_linter()
  msg <- "There should be a space between the right parenthesis and the function body."
  
  # No space after the closing parenthesis prompts a lint
  expect_lint("function()test", msg, linter)
  expect_lint("print('hello')\nx <- function(x)NULL\nprint('hello')", msg, linter)
  
  # A space after the closing parenthesis does not prompt a lint
  expect_lint("function() test", NULL, linter)
  
  # Symbols after the closing parentheses of a function call do not prompt a lint
  expect_lint("head(mtcars)$cyl", NULL, linter)
  
  # paren_body_linter returns the correct column number
  expect_lint("function()test", list(column_number = 11L), linter)
  
  # paren_body_linter returns the correct line number
  expect_lint("function()test", list(line_number = 1L), linter)
  expect_lint(
    "print('hello')\nx <- function(x)NULL\nprint('hello')",
    list(line_number = 2L),
    linter
  )
  
  # paren_body_linter returns the correct type
  expect_lint("function()test", list(type = "style"), linter)
  
  # paren_body_linter returns the correct line
  expect_lint("function()test", list(line = "function()test"), linter)
  
  # paren_body_linter returns the correct range
  expect_lint("function()test", list(ranges = list(c(11L, 14L))), linter)
  
  # paren_body_linter does not lint when the function body is defined
  # on a new line
  expect_lint("function()\n  test", NULL, linter)
  
  # paren_body_linter does not lint comments
  expect_lint("#function()test", NULL, linter)
})
