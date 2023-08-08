test_that("keyword_quote_linter skips allowed usages", {
  linter <- keyword_quote_linter()

  # main use case: c()
  expect_lint("x <- c(1, 2, 4, 5)", NULL, linter)
  expect_lint("x <- c(a = 1, 2)", NULL, linter)
  expect_lint("x <- c(a = 1, b = 2)", NULL, linter)
  expect_lint("y <- c(`a b` = 1, `c d` = 2)", NULL, linter)
  expect_lint('y <- c("a b" = 1, "c d" = 2)', NULL, linter)
  expect_lint("z <- c('a b' = 1, c = 2)", NULL, linter)

  # don't catch strings as arguments
  expect_lint('c(A = "a")', NULL, linter)
  # don't catch unnamed arguments
  expect_lint('c(1, 2, "a")', NULL, linter)
  # don't get thrown off by missing arguments
  expect_lint("alist(`a b` =)", NULL, linter)

  # other use cases: switch() and list()
  expect_lint("list(a = 1, b = list(c = 2))", NULL, linter)
  expect_lint("list(`a b` = 1, c = 2:6)", NULL, linter)

  expect_lint("switch(x, a = 1, b = 2)", NULL, linter)
  expect_lint(
    "switch(x, `a b` = 1, c = 2:6)",
    NULL,
    linter
  )
})

test_that("keyword_quote_linter blocks simple disallowed usages", {
  linter <- keyword_quote_linter()
  lint_msg <- "Only quote named arguments to functions"

  expect_lint('c("a" = 1, b = 2)', lint_msg, linter)

  expect_lint(
    "c('a' = 1, 'b' = 2)",
    list(lint_msg, lint_msg),
    linter
  )

  expect_lint(
    "c(`a` = 1, b = list(`c` = 2))",
    list(lint_msg, lint_msg),
    linter
  )

  # missing argument is caught
  expect_lint("alist(`a` = )", lint_msg, linter)

  expect_lint(
    "switch(x, `a` = c('b' = list(\"c\" = 1)))",
    list(lint_msg, lint_msg, lint_msg),
    linter
  )
})

test_that("keyword_quote_linter skips quoting on reserved words", {
  linter <- keyword_quote_linter()

  expect_lint("c(`next` = 1, `while` = 2)", NULL, linter)
  expect_lint("switch(x, `for` = 3, `TRUE` = 4)", NULL, linter)
  expect_lint("list('NA' = 5, 'Inf' = 6)", NULL, linter)
})

test_that("keyword_quote_linter works on more common functions", {
  linter <- keyword_quote_linter()
  lint_msg <- "Only quote named arguments to functions"

  expect_lint("data.frame('a' = 1)", lint_msg, linter)
  expect_lint("data.table('a' = 1)", lint_msg, linter)
  expect_lint("data.table::data.table('a' = 1)", lint_msg, linter)
  expect_lint("rbind('a' = 1)", lint_msg, linter)
  expect_lint("cbind('a' = 1)", lint_msg, linter)
})

test_that("keyword_quote_linter finds blocked usages in any function call", {
  expect_lint(
    "foo('a' = 1)",
    rex::rex("Only quote named arguments to functions"),
    keyword_quote_linter()
  )
})

test_that("keyword_quote_linter blocks quoted assignment targets", {
  linter <- keyword_quote_linter()
  backtick_msg <- rex::rex("Use backticks to create non-syntactic names, not quotes.")
  assign_msg <- "Only quote targets of assignment if necessary"

  expect_lint('"foo bar" <- 1', backtick_msg, linter)
  expect_lint("'foo bar' = 1", backtick_msg, linter)
  # valid choice: use backticks
  expect_lint("`foo bar` = 1", NULL, linter)

  expect_lint('"foo" <- 1', assign_msg, linter)
  expect_lint("'foo' = 1", assign_msg, linter)
  expect_lint("`foo` = 1", assign_msg, linter)

  # don't include data.table assignments
  expect_lint('DT[, "a" := 1]', NULL, linter)
  expect_lint("DT[, 'a' := 1]", NULL, linter)
  expect_lint("DT[, `a` := 1]", NULL, linter)

  # include common use cases: [<-/$ methods and infixes
  expect_lint('"$.my_class" <- function(x, key) { }', backtick_msg, linter)
  expect_lint("'Setter[<-.my_class' = function(x, idx, value) { }", backtick_msg, linter)
  expect_lint('"%nin%" <- function(x, table) !x %in% table', backtick_msg, linter)

  # right assignment
  expect_lint('1 -> "foo"', assign_msg, linter)
  expect_lint("1 -> foo", NULL, linter)
  expect_lint('1 -> "a b"', backtick_msg, linter)
})

test_that("keyword_quote_linter blocks quoted $, @ extractions", {
  linter <- keyword_quote_linter()
  backtick_msg <- rex::rex("Use backticks to create non-syntactic names, not quotes.")
  dollar_msg <- rex::rex("Only quote targets of extraction with $ if necessary")
  at_msg <- rex::rex("Only quote targets of extraction with @ if necessary")

  expect_lint('x$"foo bar" <- 1', backtick_msg, linter)
  expect_lint("x$'foo bar' = 1", backtick_msg, linter)
  expect_lint('x@"foo bar" <- 1', backtick_msg, linter)
  expect_lint("x@'foo bar' = 1", backtick_msg, linter)
  # valid choice: non-syntactic name with backticks
  expect_lint("x@`foo bar` <- 1", NULL, linter)
  expect_lint("x@`foo bar` = 1", NULL, linter)

  expect_lint('x$"foo" <- 1', dollar_msg, linter)
  expect_lint("x$'foo' = 1", dollar_msg, linter)
  expect_lint('x@"foo" <- 1', at_msg, linter)
  expect_lint("x@'foo' = 1", at_msg, linter)
  expect_lint("x@`foo` <- 1", at_msg, linter)
  expect_lint("x@`foo` = 1", at_msg, linter)
})

test_that("multiple lints are generated correctly", {
  linter <- keyword_quote_linter()

  expect_lint(
    trim_some('{
      foo("a" = 1)
      "b" <- 2
      x$"c"
      y@"d"
    }'),
    list(
      "Only quote named arguments",
      "Only quote targets of assignment",
      "Only quote targets of extraction with \\$",
      "Only quote targets of extraction with @"
    ),
    linter
  )

  # multiple flavors of assignment lints
  expect_lint(
    trim_some('{
      "a" <- 1
      "a b" <- 1
      `a` <- 1
      `a b` <- 1
    }'),
    list(
      "Only quote targets of assignment if necessary",
      "Use backticks to create non-syntactic names, not quotes",
      "Only quote targets of assignment if necessary"
    ),
    linter
  )

  # multiple flavors of extraction lints
  expect_lint(
    trim_some('{
      x$"a"
      x$"a b" <- 1
      x$`a` <- 1
      x$`a b` <- 1
      y@"a"
      y@"a b" <- 1
      y@`a` <- 1
      y@`a b` <- 1
    }'),
    list(
      rex::rex("Only quote targets of extraction with $ if necessary"),
      rex::rex("Use backticks to create non-syntactic names, not quotes."),
      rex::rex("Only quote targets of extraction with $ if necessary"),
      rex::rex("Only quote targets of extraction with @ if necessary"),
      rex::rex("Use backticks to create non-syntactic names, not quotes."),
      rex::rex("Only quote targets of extraction with @ if necessary")
    ),
    linter
  )
})
