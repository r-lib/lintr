context("function_left_parentheses_linter")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, function_left_parentheses_linter)

  expect_lint("print(blah)", NULL, function_left_parentheses_linter)

  expect_lint("base::print(blah)", NULL, function_left_parentheses_linter)

  expect_lint("base::print(blah, fun(1))",
    NULL,
    function_left_parentheses_linter)

  expect_lint("blah <- function(blah) { }",
    NULL,
    function_left_parentheses_linter)

  expect_lint("(1 + 1)", NULL, function_left_parentheses_linter)

  expect_lint("( (1 + 1) )", NULL, function_left_parentheses_linter)

  expect_lint("if (blah) { }", NULL, function_left_parentheses_linter)

  expect_lint("for (i in j) { }", NULL, function_left_parentheses_linter)

  expect_lint("1 * (1 + 1)", NULL, function_left_parentheses_linter)

  expect_lint("!(1 == 1)", NULL, function_left_parentheses_linter)

  expect_lint("(2 - 1):(3 - 1)", NULL, function_left_parentheses_linter)

  expect_lint("c(1, 2, 3)[(2 - 1)]", NULL, function_left_parentheses_linter)

  expect_lint("list(1, 2, 3)[[(2 - 1)]]", NULL, function_left_parentheses_linter)

  expect_lint("range(10)[(2 - 1):(10 - 1)]", NULL, function_left_parentheses_linter)

  expect_lint("function(){function(){}}()()", NULL, function_left_parentheses_linter)

  expect_lint("c(function(){})[1]()", NULL, function_left_parentheses_linter)

  expect_lint("function(x) (mean(x) + 3)", NULL, function_left_parentheses_linter)

  expect_lint("blah (1)",
    rex("Remove spaces before the left parenthesis in a function call."),
    function_left_parentheses_linter)

  expect_lint("base::print (blah)",
    rex("Remove spaces before the left parenthesis in a function call."),
    function_left_parentheses_linter)

  expect_lint("base::print(blah, f (1))",
    rex("Remove spaces before the left parenthesis in a function call."),
    function_left_parentheses_linter)

  expect_lint("`+` (1, 1)",
    rex("Remove spaces before the left parenthesis in a function call."),
    function_left_parentheses_linter)

  expect_lint("test <- function (x) { }",
    rex("Remove spaces before the left parenthesis in a function call."),
    function_left_parentheses_linter)

  expect_lint("\"blah (1)\"",
    NULL,
    function_left_parentheses_linter)
})
