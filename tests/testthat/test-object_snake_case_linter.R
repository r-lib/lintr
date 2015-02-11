context("snake_case_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    snake_case_linter)

  expect_lint("seq_along",
    NULL,
    snake_case_linter)

  expect_lint("snake_case",
    rex("Variable and function names should not use underscores."),
    snake_case_linter)

  expect_lint("snake_case()",
    rex("Variable and function names should not use underscores."),
    snake_case_linter)


  expect_lint("pack::snake_case",
    NULL,
    snake_case_linter)

  expect_lint("pack:::snake_case",
    NULL,
    snake_case_linter)

  expect_lint("a(snake_case = 1)",
    NULL,
    snake_case_linter)
})
