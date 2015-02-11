context("camel_case_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    camel_case_linter)

  expect_lint("invokeRestartInteractively",
    NULL,
    camel_case_linter)

  expect_lint("camelCase",
    rex("Variable and function names should be all lowercase."),
    camel_case_linter)

  expect_lint("camelCase()",
    rex("Variable and function names should be all lowercase."),
    camel_case_linter)

  expect_lint("pack::camelCase",
    NULL,
    camel_case_linter)

  expect_lint("pack:::camelCase",
    NULL,
    camel_case_linter)

  expect_lint("a(camelCase = 1)",
    NULL,
    camel_case_linter)
})
