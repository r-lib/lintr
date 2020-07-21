context("indentation_linter")

test_that("indentation linter flags unindented expressions", {
  expect_lint("
      lapply(1:10, function(i) {
        i %% 2
      })
    ",
    NULL,
    indentation_linter(indent = 2L))

  expect_lint("
      lapply(1:10, function(i) {
       i %% 2  # indentation is only 1 character
      })
    ",
    "indent",
    indentation_linter(indent = 2L))

  expect_lint("
      lapply(1:10, function(i) {
          i %% 2
      })
    ",
    NULL,
    indentation_linter(indent = 4L))

  expect_lint("
      lapply(1:10, function(i) {
        i %% 2  # indentation is only 2 characters
      })
    ",
    "indent",
    indentation_linter(indent = 4L))
})

test_that("indentation linter respects outermost flag", {
  expect_lint("
      lapply(1:10, function(i) {
        {
          i %% 2
        }
      })
    ",
    NULL,
    indentation_linter(outermost_only = TRUE))

  # single Lint generated when outermost_only = TRUE
  expect_lint("
      lapply(1:10, function(i) {
         {  # improper indentation
          i %% 2  # return to proper indentation
         }
      })
    ",
    "indent",
    indentation_linter(outermost_only = TRUE))

  # multiple Lints get generated when outermost_only = FALSE
  expect_lint("
      lapply(1:10, function(i) {
         {  # improper indentation
          i %% 2  # return to proper indentation
         }
      })
    ",
    list(
      list(linter = "indentation_linter"),
      list(linter = "indentation_linter")),
    indentation_linter(outermost_only = FALSE))
})

test_that("indentation linter flags improper closing curly braces", {
  expect_lint("
      lapply(1:10, function(i) {
        {
          i %% 2
        }
      })
    ",
    NULL,
    indentation_linter(indent = 2L))

  expect_lint("
      lapply(1:10, function(i) {
        i %% 2
        } # closing curly doesn't return to parent indentation
      )
    ",
    "curly",
    indentation_linter(indent = 2L))
})

test_that("function argument indentation works in tidyverse-style", {
  expect_lint("
      function(a = 1L,
               b = 2L) {
        a + b
      }
    ",
    NULL,
    indentation_linter(indent = 2L))

  expect_lint("
      test <- function(a = 1L,
                       b = 2L) {
        a + b
      }
    ",
    NULL,
    indentation_linter(indent = 2L))

  expect_lint("
      function(a = 1L,
         b = 2L) {
        a + b
      }
    ",
    "argument",
    indentation_linter(indent = 2L))

  expect_lint("
      test  <- function(a = 1L,
         b = 2L) {
        a + b
      }
    ",
    "argument",
    indentation_linter(indent = 2L))
})


test_that("function argument indentation works in generic style", {
  expect_lint("
      function(a = 1L,
        b = 2L) {
        a + b
      }
    ",
    NULL,
    indentation_linter(indent = 2L, func_header_to_open_paren = FALSE))

  expect_lint("
      test <- function(a = 1L,
        b = 2L) {

        a + b
      }
    ",
    NULL,
    indentation_linter(indent = 2L, func_header_to_open_paren = FALSE))

  expect_lint("
      function(a = 1L,
         b = 2L) {

        a + b
      }
    ",
    "argument",
    indentation_linter(indent = 2L, func_header_to_open_paren = FALSE))

  expect_lint("
      test  <- function(a = 1L,
         b = 2L) {

        a + b
      }
    ",
    "argument",
    indentation_linter(indent = 2L, func_header_to_open_paren = FALSE))
})

test_that("closing parenthesis and newline indentation catches function calls, but not other keyworded syntax", {
  # Currently closing parenthesis for non-function keyword syntax is
  # entirely unchecked. Indentation of expressions in non-function keywords
  # is expected to follow generalized rules, indenting by one indentation.

  expect_lint("
    sum(
      1,
      2
    )
    ",
    NULL,
    indentation_linter(indent = 2L, func_call_closing_paren = TRUE))

  expect_lint("
    sum(
      1,
      2)
    ",
    "(?i)closing parenthesis",
    indentation_linter(indent = 2L, func_call_closing_paren = TRUE))

  expect_lint("
    if (1 < 2 &
      3 < 4) {
      'Yep, thats right!'
    }
    ",
    NULL,
    indentation_linter(indent = 2L, func_call_closing_paren = TRUE))

  expect_lint("
    if (1 < 2 &
        3 < 4) {
      'Yep, thats right!'
    }
    ",
    "indent",
    indentation_linter(indent = 2L, func_call_closing_paren = TRUE))

  expect_lint("
    for (i in
      seq_len(5L)) {
      print(i)
    }
    ",
    NULL,
    indentation_linter(indent = 2L, func_call_closing_paren = TRUE))

  expect_lint("
    for (i in
         seq_len(5L)) {
      print(i)
    }
    ",
    "indent",
    indentation_linter(indent = 2L, func_call_closing_paren = TRUE))

  expect_lint("
    while (i < 5L &&
      i > 0L) {
      print(i)
      i <- i + 1
    }
    ",
    NULL,
    indentation_linter(indent = 2L, func_call_closing_paren = TRUE))

  expect_lint("
    while (i < 5L &&
           i > 0L) {
      print(i)
      i <- i + 1
    }
    ",
    "indent",
    indentation_linter(indent = 2L, func_call_closing_paren = TRUE))
})
