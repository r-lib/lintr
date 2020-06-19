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
    indentation_linter(parent_only = TRUE))

  # single Lint generated when outermost_only = TRUE
  expect_lint("
      lapply(1:10, function(i) {
         {  # improper indentation
          i %% 2  # return to proper indentation
         }
      })
    ",
    "indent",
    indentation_linter(parent_only = TRUE))

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
    indentation_linter(parent_only = FALSE))
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

