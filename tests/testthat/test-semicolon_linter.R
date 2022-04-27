trail_msg <- "Trailing semicolons are not needed."
comp_msg <- "Compound semicolons are discouraged. Replace them by a newline."

test_that("Lint all semicolons", {
  linter <- semicolon_linter()

  # No semicolon
  expect_lint("", NULL, linter)
  expect_lint("a <- 1", NULL, linter)
  expect_lint("function() {a <- 1}", NULL, linter)
  expect_lint("a <- \"foo;bar\"", NULL, linter)
  expect_lint("function() {a <- \"foo;bar\"}", NULL, linter)
  expect_lint("a <- FALSE # ok; cool!", NULL, linter)
  expect_lint("function() {\na <- FALSE # ok; cool!\n}", NULL, linter)

  # Trailing semicolons
  expect_lint("a <- 1;",
              list(message = trail_msg, line_number = 1L, column_number = 7L),
              linter)
  expect_lint("function(){a <- 1;}",
              list(message = trail_msg, line_number = 1L, column_number = 18L),
              linter)
  expect_lint("a <- 1; \n",
              list(message = trail_msg, line_number = 1L, column_number = 7L),
              linter)
  expect_lint("function(){a <- 1; \n}",
              list(message = trail_msg, line_number = 1L, column_number = 18L),
              linter)

  # Compound semicolons
  expect_lint("a <- 1;b <- 2",
              list(message = comp_msg, line_number = 1L, column_number = 7L),
              linter)
  expect_lint("function() {a <- 1;b <- 2}\n",
              list(message = comp_msg, line_number = 1L, column_number = 19L),
              linter)
  expect_lint("foo <-\n   1 ; foo <- 1.23",
              list(message = comp_msg, line_number = 2L, column_number = 6L),
              linter)
  expect_lint("function(){\nfoo <-\n   1 ; foo <- 1.23\n}",
              list(message = comp_msg, line_number = 3L, column_number = 6L),
              linter)

  # Multiple, mixed semicolons", {
  expect_lint("a <- 1 ; b <- 2;\nc <- 3;",
              list(
                list(message = comp_msg, line_number = 1L, column_number = 8L),
                list(message = trail_msg, line_number = 1L, column_number = 16L),
                list(message = trail_msg, line_number = 2L, column_number = 7L)
              ),
              linter)
  expect_lint("function() { a <- 1 ; b <- 2;\nc <- 3;}",
              list(
                list(message = comp_msg, line_number = 1L, column_number = 21L),
                list(message = trail_msg, line_number = 1L, column_number = 29L),
                list(message = trail_msg, line_number = 2L, column_number = 7L)
              ),
              linter)
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

test_that("deprecation notices for semicolon_terminator_linter succeed, and the deprecated version works", {
  expect_warning(
    linter <- semicolon_terminator_linter(),
    "Linter semicolon_terminator_linter was deprecated",
    fixed = TRUE
  )
  expect_lint("a <- 1", NULL, linter)
  expect_lint("a <- 1;", rex::rex("Trailing semicolons are not needed."), linter)
  expect_lint("a <- 1; b <- 2", rex::rex("Compound semicolons are discouraged."), linter)

  # old string argument gets translated to new boolean arguments
  expect_warning(
    linter <- semicolon_terminator_linter("compound"),
    "Linter semicolon_terminator_linter was deprecated",
    fixed = TRUE
  )
  expect_lint("a <- 1", NULL, linter)
  expect_lint("a <- 1;", NULL, linter)
  expect_lint("a <- 1; b <- 2", rex::rex("Compound semicolons are discouraged."), linter)

  expect_warning(
    linter <- semicolon_terminator_linter("trailing"),
    "Linter semicolon_terminator_linter was deprecated",
    fixed = TRUE
  )
  expect_lint("a <- 1", NULL, linter)
  expect_lint("a <- 1;", rex::rex("Trailing semicolons are not needed."), linter)
  expect_lint("a <- 1; b <- 2", NULL, linter)

  # with_defaults warns about now-absent semicolon_terminator_linter
  expect_warning(
    d <- with_defaults(semicolon_terminator_linter = NULL),
    # regex because the message uses sQuote() --> fancy quotes
    rex::rex("Trying to remove", anything, "semicolon_terminator_linter", anything, ", which is not in `default`.")
  )
  expect_true("semicolon_linter" %in% names(d))
})
