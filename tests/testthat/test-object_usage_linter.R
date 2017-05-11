context("object_usage_linter")

test_that("returns the correct linting", {
  msg_not_used <- rex("local variable", anything, "assigned but may not be used")
  msg_no_def <- rex("no visible global function definition for ", anything, ", Did you mean", anything)
  msg_no_bind <- rex("no visible binding for global variable ", anything, ", Did you mean")
  linter <- object_usage_linter()
  expect_is(linter, "linter")

  expect_lint("blah",
    NULL,
    linter)

  expect_lint(
"function() {
  a <- 1
  a
}",
    NULL,
    linter)

  expect_lint(
"fun <- function(x) {
  fun(1)
}
fun2 <- function(x) {
  fun2(2)
}",
    NULL,
    linter)

  expect_lint(
"fun <- function() {
  a <- 1
}",
    msg_not_used,
    linter)

  expect_lint(
"fun <- function() {
  a <- 1
  1
}",
    msg_not_used,
    linter)

  expect_lint(
"fun <- function() {
  a <- 1
}",
    msg_not_used,
    linter)

  expect_lint(
"fun <- function() {
  a2 <- 1
  a3
}",
    list(
      msg_not_used,
      msg_no_bind
      ),
    linter)

  expect_lint(
"fun <- function() {
  fnu(1)
}",
    msg_no_def,
    linter)

  expect_lint(
"fun <- function(x) {
  n(1)
}",
    msg_no_def,
    linter)

  test_that("replace_functions_stripped", {
    expect_lint(
"fun <- function(x) {
  n(x) = 1
}",
    msg_no_def,
    linter)

    expect_lint(
"fun <- function(x) {
  n(x) <- 1
}",
    msg_no_def,
    linter)
  })

})


test_that("eval errors are ignored", {
  expect_lint("
    setMethod(\"[[<-\", c(\"stampedEnv\", \"character\", \"missing\"),
      function(x) {
        x
      })",
    NULL,
    object_usage_linter())
})
