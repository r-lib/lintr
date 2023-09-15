test_that("duplicate_argument_linter doesn't block allowed usages", {
  linter <- duplicate_argument_linter()

  expect_lint("fun(arg = 1)", NULL, linter)
  expect_lint("fun('arg' = 1)", NULL, linter)
  expect_lint("fun(`arg` = 1)", NULL, linter)
  expect_lint("'fun'(arg = 1)", NULL, linter)
  expect_lint("(function(x, y) x + y)(x = 1)", NULL, linter)
  expect_lint("dt[i = 1]", NULL, linter)
})

test_that("duplicate_argument_linter blocks disallowed usages", {
  linter <- duplicate_argument_linter()
  lint_msg <- rex::rex("Duplicate arguments in function call.")

  expect_lint("fun(arg = 1, arg = 2)", lint_msg, linter)
  expect_lint("fun(arg = 1, 'arg' = 2)", lint_msg, linter)
  expect_lint("fun(arg = 1, `arg` = 2)", lint_msg, linter)
  expect_lint("'fun'(arg = 1, arg = 2)", lint_msg, linter)
  expect_lint("(function(x, y) x + y)(x = 1, x = 2)", lint_msg, linter)
  expect_lint("dt[i = 1, i = 2]", lint_msg, linter)

  expect_lint(
    "list(
      var = 1,
      var = 2
    )",
    lint_msg,
    linter
  )
})

test_that("duplicate_argument_linter respects except argument", {
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
    list(message = rex::rex("Duplicate arguments in function call.")),
    duplicate_argument_linter(except = character())
  )

  expect_lint(
    "function(arg = 1, arg = 1) {}",
    list(message = rex::rex("Repeated formal argument 'arg'.")),
    duplicate_argument_linter(except = character())
  )
})

test_that("doesn't lint duplicated arguments in allowed functions", {
  linter <- duplicate_argument_linter()

  expect_lint(
    "x %>%
     dplyr::mutate(
       col = a + b,
       col = col + d
     )",
    NULL,
    linter
  )

  expect_lint(
    "x %>%
     dplyr::transmute(
       col = a + b,
       col = col / 2.5
     )",
    NULL,
    linter
  )

  skip_if_not_r_version("4.1.0")
  expect_lint(
    "x |>
    dplyr::mutate(
      col = col |> str_replace('t', '') |> str_replace('\\\\s+$', 'xxx')
    )",
    NULL,
    linter
  )
})
