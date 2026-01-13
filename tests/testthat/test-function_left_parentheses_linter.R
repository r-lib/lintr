test_that("function_left_parentheses_linter skips allowed usages", {
  linter <- function_left_parentheses_linter()

  expect_no_lint("blah", linter)
  expect_no_lint("print(blah)", linter)
  expect_no_lint('"print"(blah)', linter)
  expect_no_lint("base::print(blah)", linter)
  expect_no_lint('base::"print"(blah)', linter)
  expect_no_lint("base::print(blah, fun(1))", linter)
  expect_no_lint("blah <- function(blah) { }", linter) # nofuzz
  expect_no_lint("(1 + 1)", linter)
  expect_no_lint("( (1 + 1) )", linter)
  expect_no_lint("if (blah) { }", linter)
  expect_no_lint("for (i in j) { }", linter)
  expect_no_lint("1 * (1 + 1)", linter)
  expect_no_lint("!(1 == 1)", linter)
  expect_no_lint("(2 - 1):(3 - 1)", linter)
  expect_no_lint("c(1, 2, 3)[(2 - 1)]", linter)
  expect_no_lint("list(1, 2, 3)[[(2 - 1)]]", linter)
  expect_no_lint("range(10)[(2 - 1):(10 - 1)]", linter)
  expect_no_lint("function(){function(){}}()()", linter) # nofuzz
  expect_no_lint("c(function(){})[1]()", linter) # nofuzz
  expect_no_lint("function(x) (mean(x) + 3)", linter) # nofuzz
  expect_no_lint('"blah (1)"', linter)
})

test_that("function_left_parentheses_linter blocks disallowed usages", {
  linter <- function_left_parentheses_linter()
  fun_lint_msg <- rex::rex("Remove spaces before the left parenthesis in a function definition.")
  call_lint_msg <- rex::rex("Remove spaces before the left parenthesis in a function call.")

  expect_lint("blah (1)", call_lint_msg, linter)
  expect_lint("base::print (blah)", call_lint_msg, linter)
  expect_lint("base::print(blah, f (1))", call_lint_msg, linter)
  expect_lint("`+` (1, 1)", call_lint_msg, linter)
  expect_lint("test <- function (x) { }", fun_lint_msg, linter)

  expect_lint(
    "blah  (1)",
    list(message = call_lint_msg, column_number = 5L, ranges = list(c(5L, 6L))),
    linter
  )
  expect_lint(
    "test <- function  (x) { }",
    list(message = fun_lint_msg, column_number = 17L, ranges = list(c(17L, 18L))),
    linter
  )
})

test_that("multi-line cases are handled correctly", {
  linter <- function_left_parentheses_linter()
  call_lint_msg <- rex::rex("Left parenthesis should be on the same line as the function's symbol.")
  fun_lint_msg <- rex::rex("Left parenthesis should be on the same line as the 'function' symbol.")

  expect_lint(
    trim_some("
      foo <- function
      (
        param
      ) {
        param + 1
      }
    "),
    fun_lint_msg,
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
    fun_lint_msg,
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
    call_lint_msg,
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
    call_lint_msg,
    linter
  )
})

test_that("multi-lint case works", {
  expect_lint(
    trim_some("
      if (
        y > sum
               (
          x
        ) &&
        z > mean
        (
          x
        ) &&
        a > sd (x) &&
        b > var  (x)
      ) {
        TRUE
      }
    "),
    list(
      list(
        message = "Left parenthesis should be on the same line as the function's symbol.",
        line_number = 2L,
        ranges = list(c(7L, 9L))
      ),
      list(
        message = "Left parenthesis should be on the same line as the function's symbol.",
        line_number = 6L,
        ranges = list(c(7L, 10L))
      ),
      list(
        message = "Remove spaces before the left parenthesis in a function call.",
        line_number = 10L,
        ranges = list(c(9L, 9L))
      ),
      list(
        message = "Remove spaces before the left parenthesis in a function call.",
        line_number = 11L,
        ranges = list(10L:11L)
      )
    ),
    function_left_parentheses_linter()
  )
})

test_that("it doesn't produce invalid lints", {
  # Part of #1427
  expect_no_warning(
    expect_lint(
      "function() {)",
      list(linter = "error"),
      function_left_parentheses_linter()
    )
  )
})

test_that("newline in character string doesn't trigger false positive (#1963)", {
  linter <- function_left_parentheses_linter()

  expect_no_lint('foo("\n")$bar()', linter)
  # also corrected the lint metadata for similar cases
  expect_lint(
    trim_some('
      (
        foo("
        ")$bar
        ()
      )
    '),
    # attach to 'b' in '$bar'
    list(line_number = 3L, column_number = 6L),
    linter
  )

  expect_lint(
    trim_some('
      (
        foo("
        ")@bar
        ()
      )
    '),
    # attach to 'b' in '@bar'
    list(line_number = 3L, column_number = 6L),
    linter
  )
})

test_that("shorthand functions are handled", { # nofuzz
  linter <- function_left_parentheses_linter()
  fun_lint_msg <- rex::rex("Remove spaces before the left parenthesis in a function definition.")

  expect_no_lint("blah <- \\(blah) { }", linter)
  expect_no_lint("\\(){\\(){}}()()", linter)
  expect_lint("test <- \\ (x) { }", fun_lint_msg, linter)
})
