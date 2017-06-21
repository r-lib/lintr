context("nonlocal_variable_linter")

msg <- "Variable is non-local"

code_global_1 <- "function(){
  # anonymous toplevel function
  CONSTANT_A
}
"

code_global_2 <- "function(){
  # toplevel function with right-assign
  CONSTANT_B
} -> some_func_right
"

code_global_3 <- "some_func <- function(x) {
  # regular toplevel function
  y <- x + log(a)
  return(y)
}
"

code_global_4 <- "function() {const <<- 123.456}"

code_local_1 <- "some_func <- function() {
  inner_func <- function(x) {
    dummy <- \"test\"
    if (runif(1) > 0.5) {
      y <- x + 1
    } else {
      y <- a + b - 1
    }; function(){
      dummy
    }
    y
  }
  inner_func(123)
}"

code_local_2 <- "function() {
  count <- 0
  lapply(
    1:3,
    function(x) {count <<- x + 1; count}
  )
  count
}
"


test_that("returns the correct linting", {
  linter <- nonlocal_variable_linter()

  expect_lint("", NULL, linter)

  expect_lint("function(a) {a*2L}", NULL, linter)

  expect_lint("function() {a <- a+1}", NULL, linter)
  # codetools cannot distinguish a non-local access from a local assignment to the same name

  expect_lint(
    code_global_1,
    list(message = msg, type = "warning", linter = "nonlocal_variable_linter",
        line_number = 3L, column_number = 3L),
    linter
  )

  expect_lint(
    code_global_2,
    list(message = msg, line_number = 3L, column_number = 3L),
    linter
  )

  expect_lint(
    code_global_3,
    list(message = msg, line_number = 3L, column_number = 16L),
    linter
  )

  expect_lint(
    code_global_4,
    list(message = msg, line_number = 1L, column_number = 13L),
    linter
  )

  expect_lint(
    code_local_1,
    list(
      list(message = msg, line_number = 7L, column_number = 12L),
      list(message = msg, line_number = 7L, column_number = 16L),
      list(message = msg, line_number = 9L, column_number = 7L)
    ),
    linter
  )

  expect_lint(
    code_local_2,
    list(message = msg, line_number = 5L, column_number = 18L),
    linter
  )

})


# try "global" only

# try "other" only
