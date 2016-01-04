context("spaces_left_parentheses_linter")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, spaces_left_parentheses_linter)

  expect_lint("print(blah)", NULL, spaces_left_parentheses_linter)

  expect_lint("base::print(blah)", NULL, spaces_left_parentheses_linter)

  expect_lint("base::print(blah, fun(1))",
    NULL,
    spaces_left_parentheses_linter)

  expect_lint("blah <- function(blah) { }",
    NULL,
    spaces_left_parentheses_linter)

  expect_lint("(1 + 1)", NULL, spaces_left_parentheses_linter)

  expect_lint("( (1 + 1) )", NULL, spaces_left_parentheses_linter)

  expect_lint("if (blah) { }", NULL, spaces_left_parentheses_linter)

  expect_lint("for (i in j) { }", NULL, spaces_left_parentheses_linter)

  expect_lint("1 * (1 + 1)", NULL, spaces_left_parentheses_linter)

  expect_lint("!(1 == 1)", NULL, spaces_left_parentheses_linter)

  expect_lint("(2 - 1):(3 - 1)", NULL, spaces_left_parentheses_linter)

  expect_lint("c(1, 2, 3)[(2 - 1)]", NULL, spaces_left_parentheses_linter)

  expect_lint("list(1, 2, 3)[[(2 - 1)]]", NULL, spaces_left_parentheses_linter)

  expect_lint("range(10)[(2 - 1):(10 - 1)]", NULL, spaces_left_parentheses_linter)

  expect_lint("function(){function(){}}()()", NULL, spaces_left_parentheses_linter)

  expect_lint("c(function(){})[1]()", NULL, spaces_left_parentheses_linter)

  expect_lint("((1 + 1))",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)

  expect_lint("if(blah) { }",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)

  expect_lint("for(i in j) { }",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)

  expect_lint("1*(1 + 1)",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)

  expect_lint("test <- function(x) { if(1 + 1) 'hi' }",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)

  expect_lint("test <- function(x) { if(`+`(1, 1)) 'hi' }",
    rex("Place a space before left parenthesis, except in a function call."),
    spaces_left_parentheses_linter)

  expect_lint("\"test <- function(x) { if(1 + 1) 'hi' }\"",
    NULL,
    spaces_left_parentheses_linter)
})
