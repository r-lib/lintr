test_that("returns the correct linting", {
  expect_lint("fun(arg = 1)",
    NULL,
    duplicate_argument_linter())

  expect_lint("fun('arg' = 1)",
    NULL,
    duplicate_argument_linter())

  expect_lint("fun(`arg` = 1)",
    NULL,
    duplicate_argument_linter())

  expect_lint("'fun'(arg = 1)",
    NULL,
    duplicate_argument_linter())

  expect_lint("(function(x, y) x + y)(x = 1)",
    NULL,
    duplicate_argument_linter())

  expect_lint("dt[i = 1]",
    NULL,
    duplicate_argument_linter())

  expect_lint("fun(arg = 1, arg = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter())

  expect_lint("fun(arg = 1, 'arg' = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter())

  expect_lint("fun(arg = 1, `arg` = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter())

  expect_lint("'fun'(arg = 1, arg = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter())

  expect_lint("(function(x, y) x + y)(x = 1, x = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter())

  expect_lint("dt[i = 1, i = 2]",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter())

  expect_lint(
    "list(
      var = 1,
      var = 2
    )",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter())

  expect_lint(
    "list(
      var = 1,
      var = 2
    )",
    NULL,
    duplicate_argument_linter(except = "list"))

  expect_lint(
    "(function(x, y) x + y)(x = 1)
    list(var = 1, var = 2)",
    NULL,
    duplicate_argument_linter(except = "list"))

  expect_lint("function(arg = 1, arg = 1) {}",
    list(message = rex("Repeated formal argument 'arg'.")),
    duplicate_argument_linter())
})
