test_that("returns the correct linting", {
  linter <- assignment_spaces_linter()
  msg <- rex("Assignments should only have one space before and after the operator.")

  expect_lint("fun(blah =  1)", NULL, linter)

  expect_lint("blah <- 1", NULL, linter)
  expect_lint("blah = 1", NULL, linter)

  expect_lint("blah <-1", NULL, linter)
  expect_lint("blah =1", NULL, linter)

  expect_lint("blah<- 1", NULL, linter)
  expect_lint("blah= 1", NULL, linter)

  expect_lint("blah<-1", NULL, linter)
  expect_lint("blah=1", NULL, linter)

  expect_lint("\"my  =  variable\" <- 42.0", NULL, linter)

  expect_lint("if (0 <  1) x <- 42L", NULL, linter)
  expect_lint(
    "if (0 < 1) {
      x <- 42L
    }",
    NULL,
    linter
  )
  expect_lint(
    "my = bad = variable = name <- 2.0",
    NULL,
    linter
  )

  expect_lint("blah<-  1", msg, linter)
  expect_lint("blah  <-1", msg, linter)
  expect_lint("blah <-  1", msg, linter)
  expect_lint("blah  <- 1", msg, linter)
  expect_lint("blah     <-     1", msg, linter)
  expect_lint("blah=  1", msg, linter)
  expect_lint("blah  =1", msg, linter)
  expect_lint("blah =  1", msg, linter)
  expect_lint("blah  = 1", msg, linter)
  expect_lint("blah     =     1", msg, linter)
  expect_lint("my = bad =  variable = name <- 2.0", msg, linter)
  expect_lint(
    "my = bad =  variable = name  <- 2.0",
    list(
      msg,
      msg
    ),
    linter
  )
})
