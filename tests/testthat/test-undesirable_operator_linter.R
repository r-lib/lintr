test_that("linter returns correct linting", {
  linter <- undesirable_operator_linter(op = c("$" = "As an alternative, use the `[[` accessor.", "<<-" = NA))
  msg_assign <- rex::escape("Operator `<<-` is undesirable.")
  msg_dollar <- rex::escape("Operator `$` is undesirable. As an alternative, use the `[[` accessor.")

  expect_lint("x <- foo:::getObj()", NULL, linter)
  expect_lint("cat(\"10$\")", NULL, linter)
  expect_lint("a <<- log(10)", list(message = msg_assign, line_number = 1L, column_number = 3L), linter)
  expect_lint("data$parsed == c(1, 2)", list(message = msg_dollar, line_number = 1L, column_number = 5L), linter)
})

test_that("undesirable_operator_linter handles '=' consistently", {
  linter <- undesirable_operator_linter(op = c("=" = "As an alternative, use '<-'"))

  expect_lint("a = 2L", rex::rex("Operator `=` is undesirable."), linter)
  expect_lint("lm(data = mtcars)", NULL, linter)
  expect_lint("function(a = 1) { }", NULL, linter)
})

test_that("undesirable_operator_linter handles infixes correctly", {
  linter <- undesirable_operator_linter(list("%oo%" = NA))
  expect_lint("a %oo% b", rex::rex("Operator `%oo%` is undesirable"), linter)
  expect_lint("a %00% b", NULL, linter)

  # somewhat special case: %% is in infix_metadata
  expect_lint(
    "foo(x %% y, x %/% y)",
    rex::rex("Operator `%%` is undesirable"),
    undesirable_operator_linter(list("%%" = NA))
  )
})

test_that("undesirable_operator_linter vectorizes messages", {
  expect_lint(
    "x <<- c(pkg:::foo, bar %oo% baz)",
    list(
      rex::rex("`<<-` is undesirable. It assigns"),
      rex::rex("`:::` is undesirable. It accesses"),
      rex::rex("`%oo%` is undesirable.", end)
    ),
    undesirable_operator_linter(modify_defaults(default_undesirable_operators, "%oo%" = NA))
  )
})
