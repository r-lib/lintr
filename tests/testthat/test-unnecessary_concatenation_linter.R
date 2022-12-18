test_that("unnecessary_concatenation_linter skips allowed usages", {
  linter <- unnecessary_concatenation_linter()

  expect_lint("c(x)", NULL, linter)
  expect_lint("c(1, 2)", NULL, linter)
  expect_lint("c(x, recursive = TRUE)", NULL, linter)
  expect_lint("c(1, recursive = FALSE)", NULL, linter)
  expect_lint("lapply(1, c)", NULL, linter)
  expect_lint("c(a = 1)", NULL, linter)
  expect_lint("c('a' = 1)", NULL, linter)
})

test_that("unnecessary_concatenation_linter blocks disallowed usages", {
  linter <- unnecessary_concatenation_linter()
  msg_c <- rex::escape('Unneeded concatenation of a constant. Remove the "c" call.')
  msg_e <- rex::escape('Unneeded concatenation without arguments. Replace the "c" call by NULL')

  expect_lint(
    "c()",
    list(message = msg_e, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "c(NULL)",
    list(message = msg_c, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "c(1)",
    list(message = msg_c, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "c (\n'a' )",
    list(message = msg_c, line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "c(y, c('c('),\nc())",
    list(
      list(message = msg_c, line_number = 1L, column_number = 6L),
      list(message = msg_e, line_number = 2L, column_number = 1L)
    ),
    linter
  )
})

test_that("Correctly handles concatenation within magrittr pipes", {
  linter <- unnecessary_concatenation_linter()
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

test_that("Correctly handles concatenation within native pipes", {
  skip_if_not_r_version("4.1.0")
  linter <- unnecessary_concatenation_linter()
  expect_lint('"a" |> c("b")', NULL, linter)
  expect_lint(
    '"a" |> c()',
    "Unneeded concatenation of a constant",
    linter
  )
  expect_lint(
    '"a" |> list("b", c())',
    "Unneeded concatenation without arguments",
    linter
  )
})

test_that("symbolic expressions are allowed, except by request", {
  expect_lint("c(alpha / 2)", NULL, unnecessary_concatenation_linter())
  expect_lint("c(paste0('.', 1:2))", NULL, unnecessary_concatenation_linter())
  expect_lint("c(DF[cond > 1, col])", NULL, unnecessary_concatenation_linter())

  # allow_single_expression = FALSE turns both into lints
  linter <- unnecessary_concatenation_linter(allow_single_expression = FALSE)
  message <- "Unneeded concatenation of a simple expression"
  expect_lint("c(alpha / 2)", message, linter)
  expect_lint("c(paste0('.', 1:2))", message, linter)
  expect_lint("c(DF[cond > 1, col])", message, linter)
})

test_that("sequences with : are linted whenever a constant is involved", {
  linter <- unnecessary_concatenation_linter()
  expect_lint("c(1:10)", "Unneeded concatenation of a constant", linter)
  expect_lint("c(1:sum(x))", "Unneeded concatenation of a constant", linter)

  # this is slightly different if a,b are factors, in which case : does
  #   something like interaction
  expect_lint("c(a:b)", NULL, linter)
  expect_lint("c(a:foo(b))", NULL, linter)
  expect_lint(
    "c(a:b)",
    "Unneeded concatenation of a simple expression",
    unnecessary_concatenation_linter(allow_single_expression = FALSE)
  )
  expect_lint(
    "c(a:foo(b))",
    "Unneeded concatenation of a simple expression",
    unnecessary_concatenation_linter(allow_single_expression = FALSE)
  )
})

test_that("c(...) does not lint under !allow_single_expression", {
  expect_lint("c(...)", NULL, unnecessary_concatenation_linter(allow_single_expression = FALSE))
})

test_that("invalid allow_single_expression argument produce informative error messages", {
  expect_error(
    expect_lint("c()", NULL, unnecessary_concatenation_linter(allow_single_expression = 1.0)),
    rex::rex("is.logical(allow_single_expression) is not TRUE")
  )

  expect_error(
    expect_lint("c()", NULL, unnecessary_concatenation_linter(allow_single_expression = c(TRUE, FALSE))),
    rex::rex("length(allow_single_expression) == 1L is not TRUE")
  )
})
