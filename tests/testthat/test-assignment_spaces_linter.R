context("assignment_spaces")
options(encoding = "UTF-8")
test_that("returns the correct linting", {
  expect_lint("fun(blah =  1)", NULL, assignment_spaces)

  expect_lint(
    "blah<-  1",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
  expect_lint(
    "blah  <-1",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
  expect_lint(
    "blah <-  1",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
  expect_lint(
    "blah  <- 1",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
  expect_lint(
    "blah     <-     1",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
  expect_lint(
    "blah=  1",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
  expect_lint(
    "blah  =1",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
  expect_lint(
    "blah =  1",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
  expect_lint(
    "blah  = 1",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
  expect_lint(
    "blah     =     1",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
})
