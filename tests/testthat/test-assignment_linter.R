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

test_that("other operators are caught as well", {
  expect_lint("1 -> blah", rex::rex("Use <-, not ->, for assignment."), assignment_linter())

  # <<- and ->> are only blocked optionally
  expect_lint("1 <<- blah", NULL, assignment_linter())
  expect_lint("1 ->> blah", NULL, assignment_linter())
  linter <- assignment_linter(block_double_assign = TRUE)
  expect_lint("1 <<- blah", rex::rex("Use <-, not <<-, for assignment. Assign to specific environments instead."), linter)
  expect_lint("1 ->> blah", rex::rex("Use <-, not ->>, for assignment. Assign to specific environments instead."), linter)
})
