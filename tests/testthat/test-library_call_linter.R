test_that("library_call_linter skips allowed usages", {
  linter <- library_call_linter()

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
    "),
    NULL,
    linter
  )

  expect_lint("print('test')",
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      # comment
      library(dplyr)
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      print('test')
      # library(dplyr)
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      suppressPackageStartupMessages({
        library(dplyr)
        library(knitr)
      })
    "),
    NULL,
    linter
  )
})

test_that("library_call_linter warns on disallowed usages", {
  linter <- library_call_linter()
  lint_message <- rex::rex("Move all library calls to the top of the script.")

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
      library(tidyr)
    "),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
      library(tidyr)
      library(purrr)
    "),
    list(
      list(lint_message, line_number = 3L, column_number = 1L),
      list(lint_message, line_number = 4L, column_number = 1L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
      print('test')
      library(tidyr)
    "),
    lint_message,
    linter
  )

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
      library(tidyr)
      print('test')
    "),
    lint_message,
    linter
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
      list(lint_message, line_number = 3L, column_number = 1L),
      list(lint_message, line_number = 5L, column_number = 1L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      library(dplyr)
      print('test')
      suppressMessages(library(tidyr))
      print('test')
    "),
    lint_message,
    linter
  )
})

test_that("require() treated the same as library()", {
  linter <- library_call_linter()
  lint_message_library <- rex::rex("Move all library calls to the top of the script.")
  lint_message_require <- rex::rex("Move all require calls to the top of the script.")

  expect_lint(
    trim_some('
      library(dplyr)
      require("tidyr")
    '),
    NULL,
    linter
  )

  expect_lint(
    trim_some('
      library(dplyr)
      print(letters)
      require("tidyr")
    '),
    list(lint_message_require, line_number = 3L),
    linter
  )

  expect_lint(
    trim_some('
      library(dplyr)
      print(letters)
      library(dbplyr)
      require("tidyr")
    '),
    list(
      list(lint_message_library, line_number = 3L),
      list(lint_message_require, line_number = 4L)
    ),
    linter
  )
})

test_that("allow_preamble applies as intended", {
  linter_preamble <- library_call_linter(allow_preamble = TRUE)
  linter_no_preamble <- library_call_linter(allow_preamble = FALSE)
  lint_msg <- rex::rex("Move all library calls to the top of the script.")

  lines <- trim_some("
    opts_chunk$set(eval = FALSE)
    library(dplyr)
    library(knitr)

    print(letters)
  ")
  expect_lint(lines, NULL, linter_preamble)
  expect_lint(lines, list(list(line_number = 2L), list(line_number = 3L)), linter_no_preamble)

  lines <- trim_some("
    opts_chunk$set(eval = FALSE)
    suppressPackageStartupMessages({
      library(dplyr)
      library(knitr)
    })

    print(letters)
  ")
  expect_lint(lines, NULL, linter_preamble)
  expect_lint(lines, list(list(line_number = 3L), list(line_number = 4L)), linter_no_preamble)

  lines <- trim_some("
    opts_chunk$set(eval = FALSE)
    suppressPackageStartupMessages(library(dplyr))
    library(knitr)

    print(letters)
  ")
  expect_lint(lines, NULL, linter_preamble)
  expect_lint(lines, list(list(line_number = 2L), list(line_number = 3L)), linter_no_preamble)

  lines <- trim_some("
    opts_chunk$set(eval = FALSE)
    library(dplyr)
    suppressPackageStartupMessages(library(knitr))

    print(letters)
  ")
  expect_lint(lines, NULL, linter_preamble)
  expect_lint(lines, list(list(line_number = 2L), list(line_number = 3L)), linter_no_preamble)

  lines <- trim_some("
    fun()
    library(moreFun)
    oops()
  ")
  expect_lint(lines, NULL, linter_preamble)
  expect_lint(lines, lint_msg, linter_no_preamble)
})
