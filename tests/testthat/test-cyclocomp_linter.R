test_that("returns the correct linting", {
  cc_linter_1 <- cyclocomp_linter(1)
  cc_linter_2 <- cyclocomp_linter(2)
  msg <- rex("functions should have cyclomatic complexity")

  expect_lint("if(TRUE) 1 else 2", NULL, cc_linter_2)
  expect_lint("if(TRUE) 1 else 2", msg, cc_linter_1)

  expect_lint(
    "function(x) {not parsing}",
    "unexpected symbol", cc_linter_2
  )

  complexity <- readLines(
    system.file("example/complexity.R", package = "lintr")
  )
  expect_lint(complexity, msg, cc_linter_2)
  expect_lint(
    complexity,
    "should have cyclomatic complexity of less than 2, this has 10",
    cc_linter_2
  )
  expect_lint(complexity, NULL, cyclocomp_linter(10))
})
