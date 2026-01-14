# nofuzz start
test_that("spaces_left_parentheses_linter skips allowed usages", {
  linter <- spaces_left_parentheses_linter()

  expect_no_lint("blah", linter)
  expect_no_lint("print(blah)", linter)
  expect_no_lint("base::print(blah)", linter)
  expect_no_lint("base::print(blah, fun(1))", linter)
  expect_no_lint("blah <- function(blah) { }", linter)

  expect_no_lint("(1 + 1)", linter)
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
  expect_no_lint("function(){function(){}}()()", linter)
  expect_no_lint("c(function(){})[1]()", linter)

  expect_no_lint("\"test <- function(x) { if(1 + 1) 'hi' }\"", linter)
  expect_no_lint("res <- c((mat - 1L) %*% combs + 1L)", linter)
  expect_no_lint("if (!(foo && bar || baz)) { foo }", linter)
  expect_no_lint("x^(y + z)", linter)
  expect_no_lint("x**(y + z)", linter)
  expect_no_lint("a <- -(b)", linter)

  expect_no_lint("(3^(3 + 2))", linter)
  expect_no_lint("-(!!!symb)", linter)

  expect_no_lint("'[[<-.data.frame'(object, y)", linter)
  expect_no_lint("object@data@get('input')", linter)
  expect_no_lint("x <- ~(. + y)", linter)
  # the internal newline is required to trigger the lint
  expect_no_lint("if (x > 1)\n  x <- x[-(i)]", linter)
  # these don't violate the linter, even if they are strange coding practice
  expect_no_lint("for (ii in 1:10) next()", linter)
  expect_no_lint("for (ii in 1:10) break()", linter)
})

test_that("spaces_left_parentheses_linter blocks disallowed usages", {
  linter <- spaces_left_parentheses_linter()
  lint_msg <- rex::rex("Place a space before left parenthesis, except in a function call.")

  expect_lint("if(blah) { }", lint_msg, linter)
  expect_lint("for(i in j) { }", lint_msg, linter)
  expect_lint("1*(1 + 1)", lint_msg, linter)
  expect_lint("test <- function(x) { if(1 + 1) 'hi' }", lint_msg, linter)
  expect_lint("test <- function(x) { if(`+`(1, 1)) 'hi' }", lint_msg, linter)

  # more complicated cases for parse tree
  expect_lint("y1<-(abs(yn)>90)*1", lint_msg, linter)
  expect_lint("c(a,(a+b))", lint_msg, linter)
  expect_lint("if (x>y) 1 else(2)", lint_msg, linter)
  expect_lint("y~(x+z)", lint_msg, linter)
  expect_lint("if (x>y) {(x+y) / (x-y)}", lint_msg, linter)
  expect_lint("for (ii in(1:10)) { }", lint_msg, linter)
  expect_lint("x = 1;(x + 2)*3", lint_msg, linter)
  expect_lint("foo <- function(x=(1+2)) { }", lint_msg, linter)
})

test_that("doesn't produce a warning", {
  # this contains a function with nested if-statements where the conditional includes a unary minus.
  # This specific constellation with another if-statement at the same nesting level on the other enclosing if-branch
  # caused a warning in spaces_left_parentheses_linter (e.g. 84bc3a is broken)
  complex_lines <- trim_some("
    complexity <- function(x) {
      if (x > 0.0) {
        if (x > 10.0) {
          if (x > 20.0) {
            x <- x / 2.0
          } else {
            return(x)
          }
        } else {
          return(x)
        }
      } else {
        if (x < -10.0) {
          if (x < -20.0) {
            x <- x * 2.0
          } else {
            return(x)
          }
        } else {
          return(x)
        }
      }
      x
    }
  ")

  expect_no_warning(lint(text = complex_lines, linters = spaces_left_parentheses_linter()))
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Place a space before left parenthesis, except in a function call.")

  expect_lint(
    trim_some("{
      y1<-(abs(yn)>90)*1
      for(i in j) { }
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    spaces_left_parentheses_linter()
  )
})
# nofuzz end
