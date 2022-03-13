test_that("returns the correct linting", {
  linter <- assignment_linter()
  msg <- rex::rex("Use <-, not =, for assignment.")

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

test_that("arguments handle <<- and ->/->> correctly", {
  expect_lint("1 -> blah", rex::rex("Use <-, not ->, for assignment."), assignment_linter())
  expect_lint("1 ->> blah", rex::rex("->> can have hard-to-predict behavior;"), assignment_linter())

  # <<- is only blocked optionally
  expect_lint("1 <<- blah", NULL, assignment_linter())
  expect_lint(
    "1 <<- blah",
    rex::rex("<<- can have hard-to-predict behavior;"),
    assignment_linter(allow_cascading_assign = FALSE)
  )

  # blocking -> can be disabled
  expect_lint("1 -> blah", NULL, assignment_linter(allow_right_assign = TRUE))
  expect_lint("1 ->> blah", NULL, assignment_linter(allow_right_assign = TRUE))
  # blocked under cascading assign but not under right assign --> blocked
  expect_lint(
    "1 ->> blah",
    rex::rex("->> can have hard-to-predict behavior;"),
    assignment_linter(allow_cascading_assign = FALSE, allow_right_assign = TRUE)
  )
})
