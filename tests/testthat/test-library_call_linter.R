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
      require(tidyr)
    '),
    NULL,
    linter
  )

  expect_lint(
    trim_some('
      library(dplyr)
      print(letters)
      require(tidyr)
    '),
    list(lint_message_require, line_number = 3L),
    linter
  )

  expect_lint(
    trim_some('
      library(dplyr)
      print(letters)
      library(dbplyr)
      require(tidyr)
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

test_that("skips allowed usages of library()/character.only=TRUE", {
  linter <- library_call_linter()

  expect_lint("library(data.table)", NULL, linter)
  expect_lint("function(pkg) library(pkg, character.only = TRUE)", NULL, linter)
  expect_lint("function(pkgs) sapply(pkgs, require, character.only = TRUE)", NULL, linter)
})

test_that("blocks disallowed usages of strings in library()/require()", {
  linter <- library_call_linter()

  expect_lint(
    'library("data.table")',
    rex::rex("Use symbols, not strings, in library calls."),
    linter
  )

  expect_lint(
    'library("data.table", character.only = TRUE)',
    rex::rex("Use symbols in library calls", anything, "character.only"),
    linter
  )

  expect_lint(
    'suppressWarnings(library("data.table", character.only = TRUE))',
    rex::rex("Use symbols in library calls", anything, "character.only"),
    linter
  )

  expect_lint(
    "do.call(library, list(data.table))",
    rex::rex("Call library() directly, not vectorized with do.call()"),
    linter
  )

  expect_lint(
    'do.call("library", list(data.table))',
    rex::rex("Call library() directly, not vectorized with do.call()"),
    linter
  )

  expect_lint(
    'lapply("data.table", library, character.only = TRUE)',
    rex::rex("Call library() directly, not vectorized with lapply()"),
    linter
  )

  expect_lint(
    'purr::map("data.table", library, character.only = TRUE)',
    rex::rex("Call library() directly, not vectorized with map()"),
    linter
  )
})

test_that("character.only=TRUE is caught with multiple-line source", {
  expect_lint(
    trim_some('
      suppressWarnings(library(
        "data.table",
        character.only = TRUE
      ))
    '),
    rex::rex("Use symbols in library calls", anything, "character.only"),
    library_call_linter()
  )
})

test_that("character.only=TRUE is caught inside purrr::walk as well", {
  expect_lint(
    'purr::walk("data.table", library, character.only = TRUE)',
    rex::rex("Call library() directly, not vectorized with walk()"),
    library_call_linter()
  )
})

test_that("multiple lints are generated correctly", {
  expect_lint(
    trim_some('{
      library("dplyr", character.only = TRUE)
      print("not a library call")
      require("gfile")
      sapply(pkg_list, "library", character.only = TRUE)
      purrr::walk(extra_list, require, character.only = TRUE)
    }'),
    list(
      list(message = rex::rex("library calls", anything, "character.only")),
      list(message = rex::rex("Move all require calls to the top of the script.")),
      list(message = "symbols, not strings, in require calls"),
      list(message = rex::rex("library() directly", anything, "vectorized with sapply()")),
      list(message = rex::rex("require() directly", anything, "vectorized with walk()"))
    ),
    library_call_linter()
  )
})
