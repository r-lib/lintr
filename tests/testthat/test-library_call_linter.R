test_that("library_call_linter skips allowed usages", {

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
    "),
    NULL,
    library_call_linter()
  )

  expect_lint("print('test')",
    NULL,
    library_call_linter()
  )

  expect_lint(
    trim_some("
      # comment
      library(dplyr)
    "),
    NULL,
    library_call_linter()
  )

  expect_lint(
    trim_some("
      print('test')
      # library(dplyr)
    "),
    NULL,
    library_call_linter()
  )
})

test_that("library_call_linter warns on disallowed usages", {
  lint_message <- rex::rex("Move all library calls to the top of the script.")

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
      library(tidyr)
    "),
    lint_message,
    library_call_linter()
  )

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
      library(tidyr)
      library(purrr)
    "),
    list(
      list(lint_message, line_number = 3, column_number = 1),
      list(lint_message, line_number = 4, column_number = 1)
    ),
    library_call_linter()
  )

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
      print('test')
      library(tidyr)
    "),
    lint_message,
    library_call_linter()
  )

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
      library(tidyr)
      print('test')
    "),
    lint_message,
    library_call_linter()
  )

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
      library(tidyr)
      print('test')
      library(purrr)
    "),
    list(
      list(lint_message, line_number = 3, column_number = 1),
      list(lint_message, line_number = 5, column_number = 1)
    ),
    library_call_linter()
  )
})
