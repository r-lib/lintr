context("undesirable_operator_linter")

test_that("linter returns correct linting", {
  linter <- undesirable_operator_linter(op=c("$"="use the `[[` accessor", "<<-"=NA))
  msgA <- rex::escape("Operator `<<-` is undesirable.")
  msgD <- rex::escape("Operator `$` is undesirable. As an alternative, use the `[[` accessor.")

  expect_lint("x <- foo:::getObj()", NULL, linter)
  expect_lint("cat(\"10$\")", NULL, linter)
  expect_lint("a <<- log(10)", list(message=msgA, line_number=1L, column_number=3L), linter)
  expect_lint("data$parsed == c(1, 2)", list(message=msgD, line_number=1L, column_number=5L), linter)
})
