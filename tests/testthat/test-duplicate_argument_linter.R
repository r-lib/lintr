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
  lint_msg <- rex::rex("Avoid duplicate arguments in function calls.")

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
  linter_list <- duplicate_argument_linter(except = "list")
  linter_all <- duplicate_argument_linter(except = character())

  expect_lint(
    "list(
      var = 1,
      var = 2
    )",
    NULL,
    linter_list
  )

  expect_lint(
    "(function(x, y) x + y)(x = 1)
    list(var = 1, var = 2)",
    NULL,
    linter_list
  )

  expect_lint(
    "fun(`
` = 1, `
` = 2)",
    rex::rex("Avoid duplicate arguments in function calls."),
    linter_all
  )

  expect_lint(
    "function(arg = 1, arg = 1) {}",
    rex::rex("Repeated formal argument 'arg'."),
    linter_all
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

test_that("lints vectorize", {
  lint_msg <- rex::rex("Avoid duplicate arguments")

  expect_lint(
    trim_some("{
      c(a = 1, a = 2, a = 3)
      list(b = 1, b = 2, b = 3)
    }"),
    list(
      list(lint_msg, line_number = 2L, column_number = 10L),
      list(lint_msg, line_number = 2L, column_number = 17L),
      list(lint_msg, line_number = 3L, column_number = 13L),
      list(lint_msg, line_number = 3L, column_number = 20L)
    ),
    duplicate_argument_linter()
  )
})
