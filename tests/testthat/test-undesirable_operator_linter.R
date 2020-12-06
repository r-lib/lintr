test_that("linter returns correct linting", {
  linter <- undesirable_operator_linter(op = c("$" = "use the `[[` accessor", "<<-" = NA))
  msg_assign <- rex::escape("Operator `<<-` is undesirable.")
  msg_dollar <- rex::escape("Operator `$` is undesirable. As an alternative, use the `[[` accessor.")

  expect_lint("x <- foo:::getObj()", NULL, linter)
  expect_lint("cat(\"10$\")", NULL, linter)
  expect_lint("a <<- log(10)", list(message = msg_assign, line_number = 1L, column_number = 3L), linter)
  expect_lint("data$parsed == c(1, 2)", list(message = msg_dollar, line_number = 1L, column_number = 5L), linter)
})
