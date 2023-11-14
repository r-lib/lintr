test_that("character_only_linter skips allowed usages", {
  linter <- character_only_linter()

  expect_lint("library(data.table)", NULL, linter)
  expect_lint("function(pkg) library(pkg, character.only = TRUE)", NULL, linter)
  expect_lint("function(pkgs) sapply(pkgs, require, character.only = TRUE)", NULL, linter)
})

test_that("character_only_linter blocks disallowed usages", {
  linter <- character_only_linter()

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

test_that("Printing character_only_linter works with multiple-line source", {
  expect_lint(
    trim_some('
      suppressWarnings(library(
        "data.table",
        character.only = TRUE
      ))
    '),
    rex::rex("Use symbols in library calls", anything, "character.only"),
    character_only_linter()
  )
})

test_that("character_only_linter catches purrr::walk as well", {
  expect_lint(
    'purr::walk("data.table", library, character.only = TRUE)',
    rex::rex("Call library() directly, not vectorized with walk()"),
    character_only_linter()
  )
})

test_that("multiple lints are generated correctly", {
  expect_lint(
    trim_some('{
      library("dplyr", character.only = TRUE)
      require("gfile")
      sapply(pkg_list, "library", character.only = TRUE)
      purrr::walk(extra_list, require, character.only = TRUE)
    }'),
    list(
      list(message = rex::rex("library calls", anything, "character.only")),
      list(message = "symbols, not strings, in require calls"),
      list(message = rex::rex("library() directly", anything, "vectorized with sapply()")),
      list(message = rex::rex("require() directly", anything, "vectorized with walk()"))
    ),
    character_only_linter()
  )
})
