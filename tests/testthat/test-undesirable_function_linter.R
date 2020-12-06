test_that("linter returns correct linting", {
  linter <- undesirable_function_linter(fun = c("return" = NA, "log10" = "use log()"))
  msg_return <- "Function \"return\" is undesirable.$"
  msg_log10 <- "Function \"log10\" is undesirable. As an alternative, use log\\(\\)."

  expect_lint("x <- options()", NULL, linter)
  expect_lint("cat(\"Try to return\")", NULL, linter)
  expect_lint("lapply(x, log10)", list(message = msg_log10, line_number = 1L, column_number = 11L), linter)
  expect_lint("return()", list(message = msg_return, line_number = 1L, column_number = 1L), linter)
  expect_lint("function(x) {\nprint(options())\ny <- log10(x)\nreturn(y)\n}",
              list(
                list(message = msg_log10, line_number = 3L, column_number = 6L),
                list(message = msg_return, line_number = 4L, column_number = 1L)
              ),
              linter)
})

test_that("it's possible to NOT lint symbols", {
  linter <- undesirable_function_linter(
    fun = c("dir" = NA, "log10" = "use log()"),
    symbol_is_undesirable = FALSE
  )
  expect_lint("dir <- 'path/to/a/directory'", NULL, linter)
  expect_lint("lapply(x, log10)", NULL, linter)
})
