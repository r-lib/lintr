context("semicolon_terminator_linter")

test_that("Lint all semicolons", {
  msg_trail <- "Trailing semicolons are not needed."
  msg_comp <- "Compound semicolons are not needed. Replace them by a newline."

  linter <- semicolon_terminator_linter()
  expect_is(linter, "linter")

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
              c(message=msg_trail, line_number=1L, column_number=7L),
              linter)
  expect_lint("function(){a <- 1;}",
              c(message=msg_trail, line_number=1L, column_number=18L),
              linter)
  expect_lint("a <- 1; \n",
              c(message=msg_trail, line_number=1L, column_number=7L),
              linter)
  expect_lint("function(){a <- 1; \n}",
              c(message=msg_trail, line_number=1L, column_number=18L),
              linter)

  # Compound semicolons
  expect_lint("a <- 1;b <- 2",
              c(message=msg_comp, line_number=1L, column_number=7L),
              linter)
  expect_lint("function() {a <- 1;b <- 2}\n",
              c(message=msg_comp, line_number=1L, column_number=19L),
              linter)
  expect_lint("foo <-\n   1 ; foo <- 1.23",
              c(message=msg_comp, line_number=2L, column_number=6L),
              linter)
  expect_lint("function(){\nfoo <-\n   1 ; foo <- 1.23\n}",
              c(message=msg_comp, line_number=3L, column_number=6L),
              linter)

  # Multiple, mixed semicolons", {
  expect_lint("a <- 1 ; b <- 2;\nc <- 3;",
              list(
                c(message=msg_comp, line_number=1L, column_number=8L),
                c(message=msg_trail, line_number=1L, column_number=16L),
                c(message=msg_trail, line_number=2L, column_number=7L)
              ),
              linter)
  expect_lint("function() { a <- 1 ; b <- 2;\nc <- 3;}",
              list(
                c(message=msg_comp, line_number=1L, column_number=21L),
                c(message=msg_trail, line_number=1L, column_number=29L),
                c(message=msg_trail, line_number=2L, column_number=7L)
              ),
              linter)
})


test_that("Compound semicolons only", {
  linter <- semicolon_terminator_linter(semicolon = "compound")
  expect_lint("a <- 1;", NULL, linter)
  expect_lint("function(){a <- 1;}", NULL, linter)
  expect_lint("a <- 1; \n", NULL, linter)
  expect_lint("function(){a <- 1; \n}", NULL, linter)
})


test_that("Trailing semicolons only", {
  linter <- semicolon_terminator_linter(semicolon = "trailing")
  expect_lint("a <- 1;b <- 2", NULL, linter)
  expect_lint("function() {a <- 1;b <- 2}\n", NULL, linter)
  expect_lint("f <-\n 1 ;f <- 1.23", NULL, linter)
  expect_lint("function(){\nf <-\n 1 ;f <- 1.23\n}", NULL, linter)
})
