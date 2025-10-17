test_that("Lint all semicolons", {
  linter <- semicolon_linter()
  trail_msg <- rex::rex("Remove trailing semicolons.")
  comp_msg <- rex::rex("Replace compound semicolons by a newline.")

  # No semicolon
  expect_no_lint("", linter)
  expect_no_lint("a <- 1", linter)
  expect_no_lint("function() {a <- 1}", linter)
  expect_no_lint("a <- \"foo;bar\"", linter)
  expect_no_lint("function() {a <- \"foo;bar\"}", linter)
  expect_no_lint("a <- FALSE # ok; cool!", linter)
  expect_no_lint("function() {\na <- FALSE # ok; cool!\n}", linter)

  # Trailing semicolons
  expect_lint(
    "a <- 1;",
    list(message = trail_msg, line_number = 1L, column_number = 7L),
    linter
  )
  expect_lint(
    "function(){a <- 1;}",
    list(message = trail_msg, line_number = 1L, column_number = 18L),
    linter
  )
  expect_lint(
    "a <- 1; \n",
    list(message = trail_msg, line_number = 1L, column_number = 7L),
    linter
  )
  expect_lint(
    "function(){a <- 1; \n}",
    list(message = trail_msg, line_number = 1L, column_number = 18L),
    linter
  )

  # Compound semicolons
  expect_lint(
    "a <- 1;b <- 2",
    list(message = comp_msg, line_number = 1L, column_number = 7L),
    linter
  )
  expect_lint(
    "function() {a <- 1;b <- 2}\n",
    list(message = comp_msg, line_number = 1L, column_number = 19L),
    linter
  )
  expect_lint(
    "foo <-\n   1 ; foo <- 1.23",
    list(message = comp_msg, line_number = 2L, column_number = 6L),
    linter
  )
  expect_lint(
    "function(){\nfoo <-\n   1 ; foo <- 1.23\n}",
    list(message = comp_msg, line_number = 3L, column_number = 6L),
    linter
  )

  # Multiple, mixed semicolons", {
  expect_lint(
    "a <- 1 ; b <- 2;\nc <- 3;",
    list(
      list(message = comp_msg, line_number = 1L, column_number = 8L),
      list(message = trail_msg, line_number = 1L, column_number = 16L),
      list(message = trail_msg, line_number = 2L, column_number = 7L)
    ),
    linter
  )
  expect_lint(
    "function() { a <- 1 ; b <- 2;\nc <- 3;}",
    list(
      list(message = comp_msg, line_number = 1L, column_number = 21L),
      list(message = trail_msg, line_number = 1L, column_number = 29L),
      list(message = trail_msg, line_number = 2L, column_number = 7L)
    ),
    linter
  )
})


test_that("Compound semicolons only", {
  linter <- semicolon_linter(allow_trailing = TRUE)
  expect_no_lint("a <- 1;", linter)
  expect_no_lint("function(){a <- 1;}", linter)
  expect_no_lint("a <- 1; \n", linter)
  expect_no_lint("function(){a <- 1; \n}", linter)
})


test_that("Trailing semicolons only", {
  linter <- semicolon_linter(allow_compound = TRUE)
  expect_no_lint("a <- 1;b <- 2", linter)
  expect_no_lint("function() {a <- 1;b <- 2}\n", linter)
  expect_no_lint("f <-\n 1 ;f <- 1.23", linter)
  expect_no_lint("function(){\nf <-\n 1 ;f <- 1.23\n}", linter)
})


test_that("Compound semicolons only", {
  expect_error(
    lint(text = "a <- 1;", linters = semicolon_linter(allow_trailing = TRUE, allow_compound = TRUE)),
    "At least one of `allow_compound` or `allow_trailing` must be `FALSE`",
    fixed = TRUE
  )
})
