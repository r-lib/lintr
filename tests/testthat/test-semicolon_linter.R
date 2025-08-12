test_that("Lint all semicolons", {
  linter <- semicolon_linter()
  trail_msg <- rex::rex("Remove trailing semicolons.")
  comp_msg <- rex::rex("Replace compound semicolons by a newline.")

  # No semicolon
  expect_lint("", NULL, linter)
  expect_lint("a <- 1", NULL, linter)
  expect_lint("function() {a <- 1}", NULL, linter)
  expect_lint("a <- \"foo;bar\"", NULL, linter)
  expect_lint("function() {a <- \"foo;bar\"}", NULL, linter)
  expect_lint("a <- FALSE # ok; cool!", NULL, linter)
  expect_lint("function() {\na <- FALSE # ok; cool!\n}", NULL, linter)

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
  expect_lint("a <- 1;", NULL, linter)
  expect_lint("function(){a <- 1;}", NULL, linter)
  expect_lint("a <- 1; \n", NULL, linter)
  expect_lint("function(){a <- 1; \n}", NULL, linter)
})


test_that("Trailing semicolons only", {
  linter <- semicolon_linter(allow_compound = TRUE)
  expect_lint("a <- 1;b <- 2", NULL, linter)
  expect_lint("function() {a <- 1;b <- 2}\n", NULL, linter)
  expect_lint("f <-\n 1 ;f <- 1.23", NULL, linter)
  expect_lint("function(){\nf <-\n 1 ;f <- 1.23\n}", NULL, linter)
})


test_that("Compound semicolons only", {
  expect_error(
    lint(text = "a <- 1;", linters = semicolon_linter(allow_trailing = TRUE, allow_compound = TRUE)),
    "At least one of `allow_compound` or `allow_trailing` must be `FALSE`",
    fixed = TRUE
  )
})
