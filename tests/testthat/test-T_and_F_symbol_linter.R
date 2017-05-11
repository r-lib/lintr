context("T_and_F_symbol_linter")

test_that("returns the correct linting", {
  msg_T <- "Use TRUE instead of the symbol T."
  msg_F <- "Use FALSE instead of the symbol F."
  linter <- T_and_F_symbol_linter()
  expect_is(linter, "linter")

  expect_lint("FALSE", NULL, linter)
  expect_lint("TRUE", NULL, linter)
  expect_lint("x <- \"TRUE master vs FALSE slave\"", NULL, linter)
  expect_lint("T", c(message=msg_T, line_number=1L, column_number=2L), linter)
  expect_lint("F", c(message=msg_F, line_number=1L, column_number=2L), linter)
  expect_lint("for (i in 1:10) {x <- c(T, TRUE, F, FALSE)}",
              list(
                c(message=msg_T, line_number=1L, column_number=26L),
                c(message=msg_F, line_number=1L, column_number=35L)
              ),
              linter)
})

