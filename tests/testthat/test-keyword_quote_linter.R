test_that("keyword_quote_linter skips allowed usages", {
  # main use case: c()
  expect_lint("x <- c(1, 2, 4, 5)", NULL, keyword_quote_linter())
  expect_lint("x <- c(a = 1, 2)", NULL, keyword_quote_linter())
  expect_lint("x <- c(a = 1, b = 2)", NULL, keyword_quote_linter())
  expect_lint("y <- c(`a b` = 1, `c d` = 2)", NULL, keyword_quote_linter())
  expect_lint('y <- c("a b" = 1, "c d" = 2)', NULL, keyword_quote_linter())
  expect_lint("z <- c('a b' = 1, c = 2)", NULL, keyword_quote_linter())

  # other use cases: switch() and list()
  expect_lint("list(a = 1, b = list(c = 2))", NULL, keyword_quote_linter())
  expect_lint("list(`a b` = 1, c = 2:6)", NULL, keyword_quote_linter())

  expect_lint("switch(x, a = 1, b = 2)", NULL, keyword_quote_linter())
  expect_lint(
    "switch(x, `a b` = 1, c = 2:6)",
    NULL,
    keyword_quote_linter()
  )
})

test_that("keyword_quote_linter blocks simple disallowed usages", {
  expect_lint(
    'c("a" = 1, b = 2)',
    rex::rex("Only quote named arguments to functions"),
    keyword_quote_linter()
  )

  expect_lint(
    "c('a' = 1, 'b' = 2)",
    list(
      "Only quote named arguments to functions",
      "Only quote named arguments to functions"
    ),
    keyword_quote_linter()
  )

  expect_lint(
    "c(`a` = 1, b = list(`c` = 2))",
    list(
      "Only quote named arguments to functions",
      "Only quote named arguments to functions"
    ),
    keyword_quote_linter()
  )

  expect_lint(
    "switch(x, `a` = c('b' = list(\"c\" = 1)))",
    list(
      "Only quote named arguments to functions",
      "Only quote named arguments to functions",
      "Only quote named arguments to functions"
    ),
    keyword_quote_linter()
  )
})

test_that("keyword_quote_linter skips quoting on reserved words", {
  expect_lint("c(`next` = 1, `while` = 2)", NULL, keyword_quote_linter())
  expect_lint(
    "switch(x, `for` = 3, `TRUE` = 4)",
    NULL,
    keyword_quote_linter()
  )
  expect_lint("list('NA' = 5, 'Inf' = 6)", NULL, keyword_quote_linter())
})

test_that("keyword_quote_linter works on more common functions", {
  expect_lint(
    "data.frame('a' = 1)",
    rex::rex("Only quote named arguments to functions"),
    keyword_quote_linter()
  )
  expect_lint(
    "data.table('a' = 1)",
    rex::rex("Only quote named arguments to functions"),
    keyword_quote_linter()
  )
  expect_lint(
    "data.table::data.table('a' = 1)",
    rex::rex("Only quote named arguments to functions"),
    keyword_quote_linter()
  )
  expect_lint(
    "rbind('a' = 1)",
    rex::rex("Only quote named arguments to functions"),
    keyword_quote_linter()
  )
  expect_lint(
    "cbind('a' = 1)",
    rex::rex("Only quote named arguments to functions"),
    keyword_quote_linter()
  )
})

test_that("keyword_quote_linter finds blocked usages in any function call", {
  expect_lint(
    "foo('a' = 1)",
    rex::rex("Only quote named arguments to functions"),
    keyword_quote_linter()
  )
})

test_that("keyword_quote_linter blocks quoted assignment targets", {
  expect_lint(
    '"foo bar" <- 1',
    rex::rex("Only quote targets of assignment if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    "'foo bar' = 1",
    rex::rex("Only quote targets of assignment if necessary"),
    keyword_quote_linter()
  )
  # valid choice: use backticks
  expect_lint("`foo bar` = 1", NULL, keyword_quote_linter())

  expect_lint(
    '"foo" <- 1',
    rex::rex("Only quote targets of assignment if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    "'foo' = 1",
    rex::rex("Only quote targets of assignment if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    "`foo` = 1",
    rex::rex("Only quote targets of assignment if necessary"),
    keyword_quote_linter()
  )

  # don't include data.table assignments
  expect_lint('DT[, "a" := 1]', NULL, keyword_quote_linter())
  expect_lint("DT[, 'a' := 1]", NULL, keyword_quote_linter())
  expect_lint("DT[, `a` := 1]", NULL, keyword_quote_linter())

  # include common use cases: [<-/$ methods and infixes
  expect_lint(
    '"$.my_class" <- function(x, key) { }',
    rex::rex("Only quote targets of assignment if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    "'Setter[<-.my_class' = function(x, idx, value) { }",
    rex::rex("Only quote targets of assignment if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    '"%nin%" <- function(x, table) !x %in% table',
    rex::rex("Only quote targets of assignment if necessary"),
    keyword_quote_linter()
  )
})

test_that("keyword_quote_linter blocks quoted $, @ extractions", {
  expect_lint(
    'x$"foo bar" <- 1',
    rex::rex("Only quote targets of extraction with $ if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    "x$'foo bar' = 1",
    rex::rex("Only quote targets of extraction with $ if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    'x@"foo bar" <- 1',
    rex::rex("Only quote targets of extraction with @ if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    "x@'foo bar' = 1",
    rex::rex("Only quote targets of extraction with @ if necessary"),
    keyword_quote_linter()
  )
  # valid choice: non-syntactic name with backticks
  expect_lint("x@`foo bar` <- 1", NULL, keyword_quote_linter())
  expect_lint("x@`foo bar` = 1", NULL, keyword_quote_linter())

  expect_lint(
    'x$"foo" <- 1',
    rex::rex("Only quote targets of extraction with $ if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    "x$'foo' = 1",
    rex::rex("Only quote targets of extraction with $ if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    'x@"foo" <- 1',
    rex::rex("Only quote targets of extraction with @ if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    "x@'foo' = 1",
    rex::rex("Only quote targets of extraction with @ if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    "x@`foo` <- 1",
    rex::rex("Only quote targets of extraction with @ if necessary"),
    keyword_quote_linter()
  )
  expect_lint(
    "x@`foo` = 1",
    rex::rex("Only quote targets of extraction with @ if necessary"),
    keyword_quote_linter()
  )
})

test_that("multiple lints are generated correctly", {
  expect_lint(
    trim_some('
      {
        foo("a" = 1)
        "b" <- 2
        x$"c"
        y@"d"
      }
    '),
    list(
      list(message = "Only quote named arguments"),
      list(message = "Only quote targets of assignment"),
      list(message = "Only quote targets of extraction with \\$"),
      list(message = "Only quote targets of extraction with @")
    ),
    keyword_quote_linter()
  )
})
