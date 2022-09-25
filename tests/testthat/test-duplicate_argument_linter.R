test_that("returns the correct linting", {
  expect_lint(
    "fun(arg = 1)",
    NULL,
    duplicate_argument_linter()
  )

  expect_lint(
    "fun('arg' = 1)",
    NULL,
    duplicate_argument_linter()
  )

  expect_lint(
    "fun(`arg` = 1)",
    NULL,
    duplicate_argument_linter()
  )

  expect_lint(
    "'fun'(arg = 1)",
    NULL,
    duplicate_argument_linter()
  )

  expect_lint(
    "(function(x, y) x + y)(x = 1)",
    NULL,
    duplicate_argument_linter()
  )

  expect_lint(
    "dt[i = 1]",
    NULL,
    duplicate_argument_linter()
  )

  expect_lint(
    "fun(arg = 1, arg = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter()
  )

  expect_lint(
    "fun(arg = 1, 'arg' = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter()
  )

  expect_lint(
    "fun(arg = 1, `arg` = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter()
  )

  expect_lint(
    "'fun'(arg = 1, arg = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter()
  )

  expect_lint(
    "(function(x, y) x + y)(x = 1, x = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter()
  )

  expect_lint(
    "dt[i = 1, i = 2]",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter()
  )

  expect_lint(
    "list(
      var = 1,
      var = 2
    )",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter()
  )

  expect_lint(
    "list(
      var = 1,
      var = 2
    )",
    NULL,
    duplicate_argument_linter(except = "list")
  )

  expect_lint(
    "(function(x, y) x + y)(x = 1)
    list(var = 1, var = 2)",
    NULL,
    duplicate_argument_linter(except = "list")
  )

  expect_lint(
    "fun(`
` = 1, `
` = 2)",
    list(message = rex("Duplicate arguments in function call.")),
    duplicate_argument_linter(except = character())
  )

  expect_lint(
    "function(arg = 1, arg = 1) {}",
    list(message = rex("Repeated formal argument 'arg'.")),
    duplicate_argument_linter(except = character())
  )
})

test_that("doesn't lint duplicated arguments in allowed functions", {
  expect_lint(
    "x %>%
     dplyr::mutate(
       col = a + b,
       col = col + d
     )",
    NULL,
    duplicate_argument_linter()
  )

  expect_lint(
    "x %>%
     dplyr::transmute(
       col = a + b,
       col = col / 2.5
     )",
    NULL,
    duplicate_argument_linter()
  )

  skip_if_not_r_version("4.1")
  expect_lint(
    "x |>
    dplyr::mutate(
      col = col |> str_replace('t', '') |> str_replace('\\\\s+$', 'xxx')
    )",
    NULL,
    duplicate_argument_linter()
  )
})
