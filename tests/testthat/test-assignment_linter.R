context("assignment_linter")

options(encoding = "UTF-8")
test_that("returns the correct linting", {
  msg <- rex("Use <-, not =, for assignment.")
  linter <- assignment_linter()
  expect_is(linter, "linter")

  expect_lint("blah", NULL, linter)

  expect_lint("blah <- 1", NULL, linter)

  expect_lint("blah<-1", NULL, linter)

  expect_lint("fun(blah=1)", NULL, linter)

  expect_lint("blah=1", msg, linter)

  expect_lint("blah = 1", msg, linter)

  expect_lint("blah = fun(1)", msg, linter)

  expect_lint("blah = fun(1) {",
    list(
      msg,
      c(type = "error", "unexpected")
      ),
    linter)

  expect_lint("fun((blah = fun(1)))", msg, linter)

})
