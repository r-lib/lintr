context("undesirable_function_linter")

test_that("linter returns correct linting", {
  linter <- undesirable_function_linter(fun=c("return"=NA, "log10"="use log()"))
  msgR <- "Function \"return\" is undesirable.$"
  msgL <- "Function \"log10\" is undesirable. As an alternative, use log\\(\\)."

  expect_lint("x <- options()", NULL, linter)
  expect_lint("cat(\"Try to return\")", NULL, linter)
  expect_lint("lapply(x, log10)", list(message=msgL, line_number=1L, column_number=11L), linter)
  expect_lint("return()", list(message=msgR, line_number=1L, column_number=1L), linter)
  expect_lint("function(x) {\nprint(options())\ny <- log10(x)\nreturn(y)\n}",
              list(
                list(message=msgL, line_number=3L, column_number=6L),
                list(message=msgR, line_number=4L, column_number=1L)
              ),
              linter)
})

test_that("it's possible to NOT lint symbols", {
  linter <- undesirable_function_linter(
    fun = c("dir" = NA),
    symbol_is_undesirable = FALSE
  )
  expect_lint("dir <- 1:3", NULL, linter)
})
