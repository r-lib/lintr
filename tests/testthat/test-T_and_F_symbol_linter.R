context("T_and_F_symbol_linter")

test_that("returns the correct linting", {
  linter <- T_and_F_symbol_linter
  msgT <- "Use TRUE instead of the symbol T."
  msgF <- "Use FALSE instead of the symbol F."
  expect_lint("FALSE", NULL, linter)
  expect_lint("TRUE", NULL, linter)
  expect_lint("x <- \"TRUE master vs FALSE slave\"", NULL, linter)
  expect_lint("T", list(message=msgT, line_number=1L, column_number=2L), linter)
  expect_lint("F", list(message=msgF, line_number=1L, column_number=2L), linter)
  expect_lint("for (i in 1:10) {x <- c(T, TRUE, F, FALSE)}",
              list(
                list(message=msgT, line_number=1L, column_number=26L),
                list(message=msgF, line_number=1L, column_number=35L)
              ),
              linter)
})

