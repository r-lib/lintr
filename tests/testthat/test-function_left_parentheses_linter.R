test_that("function_left_parentheses_linter skips allowed usages", {
  linter <- function_left_parentheses_linter()

  expect_lint("blah", NULL, linter)
  expect_lint("print(blah)", NULL, linter)
  expect_lint('"print"(blah)', NULL, linter)
  expect_lint("base::print(blah)", NULL, linter)
  expect_lint('base::"print"(blah)', NULL, linter)
  expect_lint("base::print(blah, fun(1))", NULL, linter)
  expect_lint("blah <- function(blah) { }", NULL, linter)
  expect_lint("(1 + 1)", NULL, linter)
  expect_lint("( (1 + 1) )", NULL, linter)
  expect_lint("if (blah) { }", NULL, linter)
  expect_lint("for (i in j) { }", NULL, linter)
  expect_lint("1 * (1 + 1)", NULL, linter)
  expect_lint("!(1 == 1)", NULL, linter)
  expect_lint("(2 - 1):(3 - 1)", NULL, linter)
  expect_lint("c(1, 2, 3)[(2 - 1)]", NULL, linter)
  expect_lint("list(1, 2, 3)[[(2 - 1)]]", NULL, linter)
  expect_lint("range(10)[(2 - 1):(10 - 1)]", NULL, linter)
  expect_lint("function(){function(){}}()()", NULL, linter)
  expect_lint("c(function(){})[1]()", NULL, linter)
  expect_lint("function(x) (mean(x) + 3)", NULL, linter)
  expect_lint("\"blah (1)\"", NULL, linter)
})

test_that("function_left_parentheses_linter blocks disallowed usages", {
  linter <- function_left_parentheses_linter()
  lint_msg <- rex::rex("Remove spaces before the left parenthesis in a function call.")

  expect_lint("blah (1)", lint_msg, linter)
  expect_lint("base::print (blah)", lint_msg, linter)
  expect_lint("base::print(blah, f (1))", lint_msg, linter)
  expect_lint("`+` (1, 1)", lint_msg, linter)
  expect_lint("test <- function (x) { }", lint_msg, linter)

  expect_lint(
    "blah  (1)",
    list(message = lint_msg, column_number = 5L, ranges = list(c(5L, 6L))),
    linter
  )
  expect_lint(
    "test <- function  (x) { }",
    list(message = lint_msg, column_number = 17L, ranges = list(c(17L, 18L))),
    linter
  )
})

test_that("multi-line cases are handled correctly", {
  linter <- function_left_parentheses_linter()
  lint_msg <- rex::rex("Remove spaces before the left parenthesis in a function call.")

  expect_lint(
    trim_some("
      foo <- function
      (
        param
      ) {
        param + 1
      }
    "),
    lint_msg,
    linter
  )

  # edge case where '(' is in the right place on the wrong line
  expect_lint(
    trim_some("
      foo <- function
                     (
        param
      ) {
        param + 1
      }
    "),
    lint_msg,
    linter
  )

  # ditto for function calls
  expect_lint(
    trim_some("
      if (
        y > sum
        (
          x
        )
      ) {
        TRUE
      }
    "),
    lint_msg,
    linter
  )
  expect_lint(
    trim_some("
      if (
        y > sum
               (
          x
        )
      ) {
        TRUE
      }
    "),
    lint_msg,
    linter
  )
})
