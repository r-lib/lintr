test_that("returns the correct linting", {
  linter <- assignment_linter()
  msg <- rex("Use <-, not =, for assignment.")

  expect_lint("blah", NULL, linter)
  expect_lint("blah <- 1", NULL, linter)
  expect_lint("blah<-1", NULL, linter)
  expect_lint("fun(blah=1)", NULL, linter)

  expect_lint("blah=1", msg, linter)
  expect_lint("blah = 1", msg, linter)
  expect_lint("blah = fun(1)", msg, linter)
  expect_lint("fun((blah = fun(1)))", msg, linter)

  expect_lint(
    "blah = fun(1) {",
    list(
      msg,
      c(type = "error", "unexpected")
    ),
    linter
  )
})
