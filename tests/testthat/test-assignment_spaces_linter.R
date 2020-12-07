test_that("returns the correct linting", {
  expect_lint("fun(blah =  1)", NULL, assignment_spaces)

  expect_lint("blah <- 1", NULL, assignment_spaces)
  expect_lint("blah = 1", NULL, assignment_spaces)

  expect_lint("blah <-1", NULL, assignment_spaces)
  expect_lint("blah =1", NULL, assignment_spaces)

  expect_lint("blah<- 1", NULL, assignment_spaces)
  expect_lint("blah= 1", NULL, assignment_spaces)

  expect_lint("blah<-1", NULL, assignment_spaces)
  expect_lint("blah=1", NULL, assignment_spaces)

  expect_lint("\"my  =  variable\" <- 42.0", NULL, assignment_spaces)

  expect_lint(
    "if (0 <  1) x <- 42L",
    NULL,
    assignment_spaces
  )
  expect_lint(
    "if (0 < 1) {
      x <- 42L
    }",
    NULL,
    assignment_spaces
  )
  expect_lint(
    "my = bad = variable = name <- 2.0",
    NULL,
    assignment_spaces
  )

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
  expect_lint(
    "my = bad =  variable = name <- 2.0",
    rex("Assignments should only have one space before and after the operator."),
    assignment_spaces
  )
  expect_lint(
    "my = bad =  variable = name  <- 2.0",
    list(
      rex("Assignments should only have one space before and after the operator."),
      rex("Assignments should only have one space before and after the operator.")
    ),
    assignment_spaces
  )
})
