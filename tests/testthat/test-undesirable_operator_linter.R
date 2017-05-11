context("undesirable_operator_linter")

test_that("linter returns correct linting", {
  msg_arrow <- rex::escape("Operator `<<-` is undesirable.")
  msg_dollar <- rex::escape("Operator `$` is undesirable. As an alternative, use the `[[` accessor.")
  linter <- undesirable_operator_linter(op=c("$"="use the `[[` accessor", "<<-"=NA))
  expect_is(linter, "linter")

  expect_lint("x <- foo:::getObj()", NULL, linter)
  expect_lint("cat(\"10$\")", NULL, linter)
  expect_lint("a <<- log(10)", c(message=msg_arrow, line_number=1L, column_number=3L), linter)
  expect_lint("data$parsed == c(1, 2)", c(message=msg_dollar, line_number=1L, column_number=5L), linter)
})
