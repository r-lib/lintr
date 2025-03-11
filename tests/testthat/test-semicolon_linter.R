# fuzzer disable: comment_injection
test_that("semicolon_linter skips allowed usages", {
  linter <- semicolon_linter()

  expect_no_lint("", linter)
  expect_no_lint("a <- 1", linter)
  expect_no_lint("function() {a <- 1}", linter)
  expect_no_lint('a <- "foo;bar"', linter)
  expect_no_lint('function() {a <- "foo;bar"}', linter)
  expect_no_lint("a <- FALSE # ok; cool!", linter)
  expect_no_lint(
    trim_some("
      function() {
        a <- FALSE # ok; cool!
      }
    "),
    linter
  )
})

test_that("semicolon_linter handles trailing semicolons", {
  linter <- semicolon_linter()
  lint_msg <- rex::rex("Remove trailing semicolons.")

  expect_lint(
    "a <- 1;",
    list(lint_msg, line_number = 1L, column_number = 7L),
    linter
  )
  expect_lint(
    "function(){a <- 1;}",
    list(lint_msg, line_number = 1L, column_number = 18L),
    linter
  )
  expect_lint(
    trim_some("
      function() { a <- 1;
      }"
    ),
    list(lint_msg, line_number = 1L, column_number = 20L),
    linter
  )
})

test_that("semicolon_linter handles compound semicolons", {
  linter <- semicolon_linter()
  lint_msg <- rex::rex("Replace compound semicolons by a newline.")

  expect_lint(
    "a <- 1;b <- 2",
    list(lint_msg, line_number = 1L, column_number = 7L),
    linter
  )
  expect_lint(
    "function() {a <- 1;b <- 2}",
    list(lint_msg, line_number = 1L, column_number = 19L),
    linter
  )
  expect_lint(
    trim_some("
      foo <-
         1 ; foo <- 1.23
    "),
    list(lint_msg, line_number = 2L, column_number = 6L),
    linter
  )
  expect_lint(
    trim_some("
      function() {
        foo <-
         1 ; foo <- 1.23
      }
    "),
    list(lint_msg, line_number = 3L, column_number = 6L),
    linter
  )
})

test_that("semicolon_linter handles multiple/mixed semicolons", {
  linter <- semicolon_linter()
  trail_msg <- rex::rex("Remove trailing semicolons.")
  comp_msg <- rex::rex("Replace compound semicolons by a newline.")

  expect_lint(
    trim_some("
      a <- 1 ; b <- 2;
      c <- 3;
    "),
    list(
      list(message = comp_msg, line_number = 1L, column_number = 8L),
      list(message = trail_msg, line_number = 1L, column_number = 16L),
      list(message = trail_msg, line_number = 2L, column_number = 7L)
    ),
    linter
  )
  expect_lint(
    trim_some("
      function() { a <- 1 ; b <- 2;
        c <- 3;}
    "),
    list(
      list(message = comp_msg, line_number = 1L, column_number = 21L),
      list(message = trail_msg, line_number = 1L, column_number = 29L),
      list(message = trail_msg, line_number = 2L, column_number = 9L)
    ),
    linter
  )
})


test_that("Compound semicolons only", {
  linter <- semicolon_linter(allow_trailing = TRUE)
  expect_no_lint("a <- 1;", linter)
  expect_no_lint("function(){a <- 1;}", linter)
  expect_no_lint(
    trim_some("
      function(){a <- 1;
      }
    "),
    linter
  )
})


test_that("Trailing semicolons only", {
  linter <- semicolon_linter(allow_compound = TRUE)
  expect_no_lint("a <- 1;b <- 2", linter)
  expect_no_lint("function() {a <- 1;b <- 2}", linter)
  expect_no_lint(
    trim_some("
      f <-
       1 ;f <- 1.23
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      function(){
        f <-
          1 ;f <- 1.23
      }
    "),
    linter
  )
})


test_that("Compound semicolons only", {
  expect_error(
    semicolon_linter(allow_trailing = TRUE, allow_compound = TRUE),
    "At least one of `allow_compound` or `allow_trailing` must be `FALSE`",
    fixed = TRUE
  )
})
# fuzzer enable: comment_injection
