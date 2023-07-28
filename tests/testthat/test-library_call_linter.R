test_that("library_call_linter skips allowed usages", {

  expect_lint(c("library(dplyr)", "print('test')"),
    NULL,
    library_call_linter()
  )

  expect_lint(c("print('test')"),
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
  expect_lint(c("library(dplyr)", "print('test')", "library(tidyr)"),
    rex("Move all library calls to the top of the script."),
    library_call_linter()
  )

  expect_lint(c("library(dplyr)", "print('test')", "print('test')", "library(tidyr)"),
    rex("Move all library calls to the top of the script."),
    library_call_linter()
  )

  expect_lint(c("library(dplyr)", "print('test')", "library(tidyr)", "print('test')"),
    rex("Move all library calls to the top of the script."),
    library_call_linter()
  )

  expect_lint(c("library(dplyr)", "print('test')", "library(tidyr)", "print('test')", "library(tidyr)"),
    rex("Move all library calls to the top of the script."),
    library_call_linter()
  )
})
