test_that("T_and_F_symbol_linter skips allowed usages", {
  linter <- T_and_F_symbol_linter()

  expect_lint("FALSE", NULL, linter)
  expect_lint("TRUE", NULL, linter)
  expect_lint("x <- \"TRUE a vs FALSE b\"", NULL, linter)
})

test_that("T_and_F_symbol_linter blocks disallowed usages", {
  linter <- T_and_F_symbol_linter()
  msg_true <- "Use TRUE instead of the symbol T."
  msg_false <- "Use FALSE instead of the symbol F."
  msg_variable_true <- "Don't use T as a variable name, as it can break code relying on T being TRUE."
  msg_variable_false <- "Don't use F as a variable name, as it can break code relying on F being FALSE."

  expect_lint("T", list(message = msg_true, line_number = 1L, column_number = 2L), linter)
  expect_lint("F", list(message = msg_false, line_number = 1L, column_number = 2L), linter)
  expect_lint("T = 42", list(message = msg_variable_true, line_number = 1L, column_number = 2L), linter)
  expect_lint("F = 42", list(message = msg_variable_false, line_number = 1L, column_number = 2L), linter)
  expect_lint(
    "for (i in 1:10) {x <- c(T, TRUE, F, FALSE)}",
    list(
      list(message = msg_true, line_number = 1L, column_number = 26L),
      list(message = msg_false, line_number = 1L, column_number = 35L)
    ),
    linter
  )

  expect_lint("DF$bool <- T", msg_true, linter)
  expect_lint("S4@bool <- T", msg_true, linter)
  expect_lint("sum(x, na.rm = T)", msg_true, linter)

  # Regression test for #657
  expect_lint(
    trim_some("
      x <- list(
        T = 42L,
        F = 21L
      )

      x$F <- 42L
      y@T <- 84L

      T <- \"foo\"
      F = \"foo2\"
      \"foo3\" -> T
    "),
    list(
      list(message = msg_variable_true, line_number = 9L),
      list(message = msg_variable_false, line_number = 10L),
      list(message = msg_variable_true, line_number = 11L)
    ),
    linter
  )
})
