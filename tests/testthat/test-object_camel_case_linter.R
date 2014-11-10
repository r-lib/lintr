context("object_camel_case_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    object_camel_case_linter)

  expect_lint("invokeRestartInteractively",
    NULL,
    object_camel_case_linter)

  expect_lint("camelCase",
    rex("Variable and function names should be all lowercase."),
    object_camel_case_linter)

  expect_lint("camelCase()",
    rex("Variable and function names should be all lowercase."),
    object_camel_case_linter)

  expect_lint("pack::camelCase",
    NULL,
    object_camel_case_linter)

  expect_lint("pack:::camelCase",
    NULL,
    object_camel_case_linter)

  expect_lint("a(camelCase = 1)",
    NULL,
    object_camel_case_linter)
})
