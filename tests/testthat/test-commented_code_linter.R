context("commented_code_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    commented_code_linter)

  expect_lint("# blah <- 1",
    rex("Commented code should be removed"),
    commented_code_linter)

  expect_lint("bleurgh <- fun_call(1) # other_call()",
    c(message = rex("Commented code should be removed"),
      column_number = 24),
    commented_code_linter)

  expect_lint(" #blah <- 1",
    c(message = rex("Commented code should be removed"),
      column_number = 2),
    commented_code_linter)

  expect_lint("#' blah <- 1",
    NULL,
    commented_code_linter)

})
