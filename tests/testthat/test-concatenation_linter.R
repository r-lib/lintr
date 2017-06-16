context("concatenation_linter")

msg_e <- rex::escape(
  "Unneded concatenation without arguments. Replace the \"c\" call by NULL or vector().")
msg_c <- rex::escape("Unneded concatenation of a constant. Remove the \"c\" call.")

test_that("returns the correct linting with default parameters", {
  linter <- concatenation_linter()

  expect_lint("c(x)", NULL, linter)
  expect_lint("c(1, 2)", NULL, linter)
  expect_lint("c(x, recursive=TRUE)", NULL, linter)
  expect_lint("c(1, recursive=FALSE)", NULL, linter)
  expect_lint("lapply(1, c)", NULL, linter)
  expect_lint("c(1, TRUE)", NULL, linter)

  expect_lint("c()", msg_e, linter)
  expect_lint("c(1)", msg_c, linter)
})

test_that("returns the correct linting for unneeded concatenations", {
  linter <- concatenation_linter("unneeded")

  expect_lint("c(1, TRUE)", NULL, linter)
  expect_lint("c()", list(message=msg_e, line_number=1L, column_number=1L), linter)
  expect_lint("c(NULL)", list(message=msg_c, line_number=1L, column_number=1L), linter)
  expect_lint("c(1)", list(message=msg_c, line_number=1L, column_number=1L), linter)
  expect_lint("c (\n'a' )", list(message=msg_c, line_number=1L, column_number=1L), linter)
  expect_lint("c(y, c('c('),\nc())",
              list(
                list(message=msg_c, line_number=1L, column_number=6L),
                list(message=msg_e, line_number=2L, column_number=1L)
              ),
              linter)
})
