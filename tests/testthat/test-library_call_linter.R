test_that("library_call_linter skips allowed usages", {

  expect_lint(c("library(dplyr)", "print('test')"),
    NULL,
    library_call_linter()
  )

  expect_lint("print('test')",
    NULL,
    library_call_linter()
  )

  expect_lint(c("# comment", "library(dplyr)"),
    NULL,
    library_call_linter()
  )

  expect_lint(c("print('test')", "# library(dplyr)"),
    NULL,
    library_call_linter()
  )
})

test_that("library_call_linter warns on disallowed usages", {
  lint_message <- rex::rex("Move all library calls to the top of the script.")

  expect_lint(c("library(dplyr)", "print('test')", "library(tidyr)"),
    lint_message,
    library_call_linter()
  )

  expect_lint(c("library(dplyr)", "print('test')", "library(tidyr)", "library(purrr)"),
    list(lint_message, lint_message),
    library_call_linter()
  )

  expect_lint(c("library(dplyr)", "print('test')", "print('test')", "library(tidyr)"),
    rex("Move all library calls to the top of the script."),
    library_call_linter()
  )

  expect_lint(c("library(dplyr)", "print('test')", "library(tidyr)", "print('test')"),
    lint_message,
    library_call_linter()
  )

  expect_lint(c("library(dplyr)", "print('test')", "library(tidyr)", "print('test')", "library(tidyr)"),
    list(lint_message, lint_message),
    library_call_linter()
  )
})
