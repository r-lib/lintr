test_that("linter returns correct linting", {
  linter <- undesirable_function_linter(fun = c(return = NA, log10 = "use log()"))
  msg_return <- rex::rex('Avoid undesirable function "return".', end)
  msg_log10 <- rex::rex('Avoid undesirable function "log10". As an alternative, use log().')

  expect_lint("x <- options()", NULL, linter)
  expect_lint("cat(\"Try to return\")", NULL, linter)
  expect_lint("lapply(x, log10)", list(message = msg_log10, line_number = 1L, column_number = 11L), linter)
  expect_lint("return()", list(message = msg_return, line_number = 1L, column_number = 1L), linter)
  expect_lint(
    trim_some("
      function(x) {
        print(options())
        y <- log10(x)
        return(y)
      }"),
    list(
      list(message = msg_log10, line_number = 3L, column_number = 8L),
      list(message = msg_return, line_number = 4L, column_number = 3L)
    ),
    linter
  )
  # regression test for #1050
  expect_lint("df$return <- 1", NULL, linter)
  expect_lint("df@return <- 1", NULL, linter)
})

test_that("it's possible to NOT lint symbols", {
  linter <- undesirable_function_linter(
    fun = c(dir = NA, log10 = "use log()"),
    symbol_is_undesirable = FALSE
  )
  expect_lint("dir <- 'path/to/a/directory'", NULL, linter)
  expect_lint("lapply(x, log10)", NULL, linter)
})

test_that("undesirable_function_linter doesn't lint library and require calls", {
  linter <- undesirable_function_linter(fun = c(foo = NA))
  expect_lint("test::foo()", "undesirable", linter)
  expect_lint("foo::test()", NULL, linter)
  expect_lint("library(foo)", NULL, linter)
  expect_lint("require(foo)", NULL, linter)

  linter <- undesirable_function_linter(fun = c(foo = NA, bar = NA))
  expect_lint("library(foo)", NULL, linter)

  linter <- undesirable_function_linter(fun = c(foo = NA, bar = NA), symbol_is_undesirable = FALSE)
  expect_lint("library(foo)", NULL, linter)
})

# regression test for #866
test_that("Line numbers are extracted correctly", {
  lines <- c(rep(letters, 10L), "tmp <- tempdir()")
  expect_lint(paste(lines, collapse = "\n"), "undesirable", undesirable_function_linter(c(tempdir = NA)))
})

test_that("invalid inputs fail correctly", {
  error_msg <- "`fun` should be a non-empty named character vector"

  expect_error(
    undesirable_function_linter("***"),
    error_msg,
    fixed = TRUE
  )
  expect_error(
    undesirable_function_linter(c("***" = NA, NA)),
    error_msg,
    fixed = TRUE
  )
  expect_error(
    undesirable_function_linter(fun = NULL),
    error_msg,
    fixed = TRUE
  )
  expect_error(
    undesirable_function_linter(fun = character(0L)),
    error_msg,
    fixed = TRUE
  )

  expect_error(
    undesirable_function_linter(symbol_is_undesirable = 1.0),
    "is.logical(symbol_is_undesirable) is not TRUE",
    fixed = TRUE
  )
})
