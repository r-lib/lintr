test_that("library_call_linter skips allowed usages", {
  linter <- library_call_linter()

  expect_no_lint(
    trim_some("
      library(dplyr)
      print('test')
    "),
    linter
  )

  expect_no_lint("print('test')", linter)

  expect_no_lint(
    trim_some("
      # comment
      library(dplyr)
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      print('test')
      # library(dplyr)
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      suppressPackageStartupMessages({
        library(dplyr)
        library(knitr)
      })
    "),
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
      suppressMessages(library('lubridate', character.only = TRUE))
      suppressMessages(library(tidyr))
      print('test')
    "),
    list(
      list(rex::rex("Unify consecutive calls to suppressMessages()"), line_number = 3L),
      list(lint_message, line_number = 3L),
      list(rex::rex("Use symbols in library calls to avoid the need for 'character.only'"), line_number = 3L),
      list(lint_message, line_number = 4L)
    ),
    linter
  )
})

test_that("require() treated the same as library()", {
  linter <- library_call_linter()
  lint_message_library <- rex::rex("Move all library calls to the top of the script.")
  lint_message_require <- rex::rex("Move all require calls to the top of the script.")

  expect_no_lint(
    trim_some("
      library(dplyr)
      require(tidyr)
    "),
    linter
  )

  expect_lint(
    trim_some("
      library(dplyr)
      print(letters)
      require(tidyr)
    "),
    list(lint_message_require, line_number = 3L),
    linter
  )

  expect_lint(
    trim_some("
      library(dplyr)
      print(letters)
      library(dbplyr)
      require(tidyr)
    "),
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
  expect_no_lint(lines, linter_preamble)
  expect_lint(lines, list(list(line_number = 2L), list(line_number = 3L)), linter_no_preamble)

  lines <- trim_some("
    opts_chunk$set(eval = FALSE)
    suppressPackageStartupMessages({
      library(dplyr)
      library(knitr)
    })

    print(letters)
  ")
  expect_no_lint(lines, linter_preamble)
  expect_lint(lines, list(list(line_number = 3L), list(line_number = 4L)), linter_no_preamble)

  lines <- trim_some("
    opts_chunk$set(eval = FALSE)
    suppressPackageStartupMessages(library(dplyr))
    library(knitr)

    print(letters)
  ")
  expect_no_lint(lines, linter_preamble)
  expect_lint(lines, list(list(line_number = 2L), list(line_number = 3L)), linter_no_preamble)

  lines <- trim_some("
    opts_chunk$set(eval = FALSE)
    library(dplyr)
    suppressPackageStartupMessages(library(knitr))

    print(letters)
  ")
  expect_no_lint(lines, linter_preamble)
  expect_lint(lines, list(list(line_number = 2L), list(line_number = 3L)), linter_no_preamble)

  # allow S4 operation to precede library() as well, equivalently to other function calls
  lines <- trim_some("
    opts_chunk@set(eval = FALSE)
    library(dplyr)
    suppressPackageStartupMessages(library(knitr))

    print(letters)
  ")
  expect_no_lint(lines, linter_preamble)
  expect_lint(lines, list(list(line_number = 2L), list(line_number = 3L)), linter_no_preamble)

  lines <- trim_some("
    fun()
    library(moreFun)
    oops()
  ")
  expect_no_lint(lines, linter_preamble)
  expect_lint(lines, lint_msg, linter_no_preamble)
})

test_that("skips allowed usages of library()/character.only=TRUE", {
  linter <- library_call_linter()

  expect_no_lint("library(data.table)", linter)
  expect_no_lint("function(pkg) library(pkg, character.only = TRUE)", linter)
  expect_no_lint("function(pkgs) sapply(pkgs, require, character.only = TRUE)", linter)

  skip_if_not_r_version("4.1.0")
  expect_no_lint("\\(pkg) library(pkg, character.only = TRUE)", linter)
  expect_no_lint("\\(pkgs) sapply(pkgs, require, character.only = TRUE)", linter)
})

test_that("blocks disallowed usages of strings in library()/require()", {
  linter <- library_call_linter()
  char_only_msg <- rex::rex("Use symbols in library calls", anything, "character.only")
  direct_stub <- "Call library() directly, not vectorized with "

  expect_lint(
    'library("data.table")',
    rex::rex("Use symbols, not strings, in library calls."),
    linter
  )

  expect_lint('library("data.table", character.only = TRUE)', char_only_msg, linter)
  expect_lint('suppressWarnings(library("data.table", character.only = TRUE))', char_only_msg, linter)

  expect_lint(
    "do.call(library, list(data.table))",
    rex::rex(direct_stub, "do.call()"),
    linter
  )
  expect_lint(
    'do.call("library", list(data.table))',
    rex::rex(direct_stub, "do.call()"),
    linter
  )
  expect_lint(
    'lapply("data.table", library, character.only = TRUE)',
    rex::rex(direct_stub, "lapply()"),
    linter
  )
  expect_lint(
    'purr::map("data.table", library, character.only = TRUE)',
    rex::rex(direct_stub, "map()"),
    linter
  )
})

test_that("character.only=TRUE is caught in *apply functions passed as strings", {
  linter <- library_call_linter()
  lib_msg <- rex::rex("Call library() directly, not vectorized with sapply()")
  req_msg <- rex::rex("Call require() directly, not vectorized with sapply()")

  expect_lint("sapply(pkgs, 'library', character.only = TRUE)", lib_msg, linter)
  expect_lint('sapply(pkgs, "library", character.only = TRUE)', lib_msg, linter)
  expect_lint("sapply(pkgs, 'require', character.only = TRUE)", req_msg, linter)
  expect_lint('sapply(pkgs, "require", character.only = TRUE)', req_msg, linter)
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

patrick::with_parameters_test_that(
  "library_call_linter skips allowed usages",
  {
    linter <- library_call_linter()

    expect_no_lint(sprintf("%s(x)", call), linter)
    expect_no_lint(sprintf("%s(x, y, z)", call), linter)

    # intervening expression
    expect_no_lint(sprintf("%1$s(x); y; %1$s(z)", call), linter)

    # inline or potentially with gaps don't matter
    expect_no_lint(
      trim_some(glue::glue("
        {call}(x)
        y

        stopifnot(z)
      ")),
      linter
    )

    # only suppressing calls with library()
    expect_no_lint(
      trim_some(glue::glue("
        {call}(x)
        {call}(y)
      ")),
      linter
    )
  },
  .test_name = c("suppressMessages", "suppressPackageStartupMessages"),
  call = c("suppressMessages", "suppressPackageStartupMessages")
)

patrick::with_parameters_test_that(
  "library_call_linter blocks simple disallowed usages",
  {
    linter <- library_call_linter()
    lint_msg <- sprintf("Unify consecutive calls to %s\\(\\)\\.", call)

    # one test of inline usage
    expect_lint(sprintf("%1$s(library(x)); %1$s(library(y))", call), lint_msg, linter)

    expect_lint(
      trim_some(glue::glue("
        {call}(library(x))

        {call}(library(y))
      ")),
      lint_msg,
      linter
    )

    expect_lint(
      trim_some(glue::glue("
        {call}(require(x))
        {call}(require(y))
      ")),
      lint_msg,
      linter
    )

    expect_lint(
      trim_some(glue::glue("
        {call}(library(x))
        # a comment on y
        {call}(library(y))
      ")),
      lint_msg,
      linter
    )
  },
  .test_name = c("suppressMessages", "suppressPackageStartupMessages"),
  call = c("suppressMessages", "suppressPackageStartupMessages")
)

test_that("Namespace differences are detected", {
  linter <- library_call_linter()

  # totally different namespaces
  expect_no_lint("ns::suppressMessages(library(x)); base::suppressMessages(library(y))", linter)

  # one namespaced, one not
  expect_no_lint("ns::suppressMessages(library(x)); suppressMessages(library(y))", linter)
})

test_that("Consecutive calls to different blocked calls is OK", {
  expect_no_lint(
    "suppressPackageStartupMessages(library(x)); suppressMessages(library(y))",
    library_call_linter()
  )
})

test_that("Multiple violations across different calls are caught", {
  linter <- library_call_linter()

  expect_lint(
    trim_some("
      suppressPackageStartupMessages(library(x))
      suppressPackageStartupMessages(library(x))
      suppressMessages(library(x))
      suppressMessages(library(x))
    "),
    list(
      "Unify consecutive calls to suppressPackageStartupMessages",
      "Unify consecutive calls to suppressMessages"
    ),
    linter
  )

  expect_lint(
    trim_some("
      suppressMessages(library(A))
      suppressPackageStartupMessages(library(A))
      suppressMessages(library(A))
      suppressPackageStartupMessages(library(A))
      suppressPackageStartupMessages(library(A))
    "),
    list("Unify consecutive calls to suppressPackageStartupMessages", line_number = 4L),
    linter
  )
})
