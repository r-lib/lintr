test_that("returns the correct linting", {
  linter <- unneeded_concatenation_linter()
  msg_c <- rex::escape("Unneeded concatenation of a constant. Remove the \"c\" call.")
  msg_e <- rex::escape(
    "Unneeded concatenation without arguments. Replace the \"c\" call by NULL or vector().")

  expect_lint("c(x)", NULL, linter)
  expect_lint("c(1, 2)", NULL, linter)
  expect_lint("c(x, recursive = TRUE)", NULL, linter)
  expect_lint("c(1, recursive = FALSE)", NULL, linter)
  expect_lint("lapply(1, c)", NULL, linter)

  expect_lint("c()", list(message = msg_e, line_number = 1L, column_number = 1L), linter)
  expect_lint("c(NULL)", list(message = msg_c, line_number = 1L, column_number = 1L), linter)
  expect_lint("c(1)", list(message = msg_c, line_number = 1L, column_number = 1L), linter)
  expect_lint("c (\n'a' )", list(message = msg_c, line_number = 1L, column_number = 1L), linter)
  expect_lint(
    "c(y, c('c('),\nc())",
    list(
      list(message = msg_c, line_number = 1L, column_number = 6L),
      list(message = msg_e, line_number = 2L, column_number = 1L)
    ),
    linter
  )
})

test_that("Correctly handles concatenation within pipes", {
  linter <- unneeded_concatenation_linter()
  expect_lint('"a" %>% c("b")', NULL, linter)
  expect_lint(
    '"a" %>% c()',
    "Unneeded concatenation of a constant",
    linter
  )
  expect_lint(
    '"a" %>% list("b", c())',
    "Unneeded concatenation without arguments",
    linter
  )
})
