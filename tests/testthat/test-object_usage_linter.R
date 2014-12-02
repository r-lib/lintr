context("object_usage_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    object_usage_linter)

  expect_lint(
"function() {
  a <- 1
  a
}",
    NULL,
    object_usage_linter)

  expect_lint(
"fun <- function(x) {
  fun(1)
}
fun2 <- function(x) {
  fun2(2)
}",
    NULL,
    object_usage_linter)

  expect_lint(
"fun <- function() {
  a <- 1
}",
    rex("local variable", anything, "assigned but may not be used"),
    object_usage_linter)

  expect_lint(
"fun <- function() {
  a <- 1
  1
}",
    rex("local variable", anything, "assigned but may not be used"),
    object_usage_linter)

  expect_lint(
"fun <- function() {
  a <- 1
}",
    rex("local variable", anything, "assigned but may not be used"),
    object_usage_linter)

  expect_lint(
"fun <- function() {
  a2 <- 1
  a3
}",
    list(
      rex("local variable", anything, "assigned but may not be used"),
      rex("no visible binding for global variable ", anything, ", Did you mean")
      ),
    object_usage_linter)

  expect_lint(
"fun <- function() {
  fnu(1)
}",
    rex("no visible global function definition for ", anything, ", Did you mean", anything),
    object_usage_linter)

  expect_lint(
"fun <- function(x) {
  n(1)
}",
    rex("no visible global function definition for ", anything, ", Did you mean", anything),
    object_usage_linter)

  test_that("replace_functions_stripped", {
    expect_lint(
"fun <- function(x) {
  n(x) = 1
}",
    rex("no visible global function definition for ", anything, ", Did you mean", anything),
    object_usage_linter)

    expect_lint(
"fun <- function(x) {
  n(x) <- 1
}",
    rex("no visible global function definition for ", anything, ", Did you mean", anything),
    object_usage_linter)
  })

})

test_that("eval errors are ignored", {
  expect_lint("
    setMethod(\"[[<-\", c(\"stampedEnv\", \"character\", \"missing\"),
      function(x) {
        x
      })",
    NULL,
    object_usage_linter)
})
