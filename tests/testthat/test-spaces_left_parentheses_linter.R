test_that("returns the correct linting", {
  linter <- spaces_left_parentheses_linter()
  msg <- rex("Place a space before left parenthesis, except in a function call.")

  expect_lint("blah", NULL, linter)
  expect_lint("print(blah)", NULL, linter)
  expect_lint("base::print(blah)", NULL, linter)
  expect_lint("base::print(blah, fun(1))", NULL, linter)
  expect_lint("blah <- function(blah) { }", NULL, linter)

  expect_lint("(1 + 1)", NULL, linter)
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

  expect_lint("if(blah) { }", msg, linter)
  expect_lint("for(i in j) { }", msg, linter)
  expect_lint("1*(1 + 1)", msg, linter)
  expect_lint("test <- function(x) { if(1 + 1) 'hi' }", msg, linter)
  expect_lint("test <- function(x) { if(`+`(1, 1)) 'hi' }", msg, linter)

  expect_lint("\"test <- function(x) { if(1 + 1) 'hi' }\"", NULL, linter)
  expect_lint("res <- c((mat - 1L) %*% combs + 1L)", NULL, linter)
  expect_lint("if (!(foo && bar || baz)) { foo }", NULL, linter)
  expect_lint("x^(y + z)", NULL, linter)
  expect_lint("x**(y + z)", NULL, linter)
  expect_lint("a <- -(b)", NULL, linter)

  # more complicated cases for parse tree
  expect_lint("y1<-(abs(yn)>90)*1", msg, linter)
  expect_lint("c(a,(a+b))", msg, linter)
  expect_lint("if (x>y) 1 else(2)", msg, linter)
  expect_lint("y~(x+z)", msg, linter)
  expect_lint("if (x>y) {(x+y) / (x-y)}", msg, linter)
  expect_lint("for (ii in(1:10)) { }", msg, linter)
  expect_lint("x = 1;(x + 2)*3", msg, linter)
  expect_lint("foo <- function(x=(1+2)) { }", msg, linter)

  expect_lint("'[[<-.data.frame'(object, y)", NULL, linter)
  expect_lint("object@data@get('input')", NULL, linter)
  expect_lint("x <- ~(. + y)", NULL, linter)
  # the internal newline is required to trigger the lint
  expect_lint("if (x > 1)\n  x <- x[-(i)]", NULL, linter)
  # these don't violate the linter, even if they are strange coding practice
  expect_lint("for (ii in 1:10) next()", NULL, linter)
  expect_lint("for (ii in 1:10) break()", NULL, linter)
})

test_that("doesn't produce a warning", {
  # complexity.R contains a function with nested if-statements where the conditional includes a unary minus.
  # This specific constellation with another if-statement at the same nesting level on the other enclosing if-branch
  # caused a warning in spaces_left_parentheses_linter (e.g. 84bc3a is broken)

  expect_silent(
    lint(
      system.file("example/complexity.R", package = "lintr")
    )
  )
})
