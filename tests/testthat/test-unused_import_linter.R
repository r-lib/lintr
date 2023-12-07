test_that("unused_import_linter lints as expected", {
  linter <- unused_import_linter()

  expect_lint("library(dplyr)\ntibble(a = 1)", NULL, linter)
  # SYMBOL_FUNCTION_CALL usage is detected
  expect_lint("library(tidyverse)\ntibble(a = 1)", NULL, linter)
  # SYMBOL usage is detected
  expect_lint("library(dplyr)\ndo.call(tibble, args = list(a = 1))", NULL, linter)
  # SPECIAL usage is detected
  expect_lint("library(magrittr)\n1:3 %>% mean()", NULL, linter)
  # dataset is detected
  expect_lint("library(dplyr)\nstarwars", NULL, linter)
  expect_lint("library(datasets)\nstate.center", NULL, linter)

  # Missing packages are ignored
  expect_lint("library(not.a.package)\ntibble(a = 1)", NULL, linter)
  # SYMBOL calls with character.only = TRUE are ignored, even if the argument is a package name
  expect_lint("library(dplyr, character.only = TRUE)\n1 + 1", NULL, linter)

  lint_msg <- rex::rex("Package 'dplyr' is attached but never used")
  msg_ns <- rex::rex("Don't attach package 'dplyr', which is only used by namespace.")

  expect_lint("library(dplyr)\n1 + 1", lint_msg, linter)
  expect_lint("require(dplyr)\n1 + 1", lint_msg, linter)
  expect_lint("library('dplyr')\n1 + 1", lint_msg, linter)
  expect_lint("library('dplyr', character.only = TRUE)\n1 + 1", lint_msg, linter)
  # ignore namespaced usages by default, but provide custom lint message
  expect_lint("library(dplyr)\ndplyr::tibble(a = 1)", msg_ns, linter)
  expect_lint("library(dplyr)\ndplyr::tibble(a = 1)", NULL, unused_import_linter(allow_ns_usage = TRUE))

  # ignore packages in except_packages
  expect_lint("library(data.table)\n1 + 1", NULL, linter)
  expect_lint("library(dplyr)\n1 + 1", NULL, unused_import_linter(except_packages = "dplyr"))
})

test_that("unused_import_linter handles message vectorization", {
  skip_if_not_installed("crayon")
  expect_lint(
    trim_some("
      library(crayon)
      library(xmlparsedata)
      xmlparsedata::xml_parse_data(parse(text = 'a'))
    "),
    list(
      list(rex::rex("Package 'crayon' is attached but never used."), line_number = 1L),
      list(rex::rex("Don't attach package 'xmlparsedata', which is only used by namespace"), line_number = 2L)
    ),
    unused_import_linter()
  )
})

test_that("unused_import_linter lints packages with exports like pkg::pkg", {
  # glue::glue is an export, so don't get thrown off by the 'glue' symbol in library()
  expect_lint(
    trim_some("
      library(glue)
      1 + 1
    "),
    rex::rex("Package 'glue' is attached but never used."),
    unused_import_linter()
  )
})

test_that("glue usages are seen", {
  lint_msg <- rex::rex("Package 'xmlparsedata' is attached but never used.")

  lines <- trim_some("
    library(glue)
    library(xmlparsedata)

    glue('{ xml_parse_data() }')
  ")
  expect_lint(lines, NULL, unused_import_linter())
  expect_lint(lines, lint_msg, unused_import_linter(interpret_glue = FALSE))
})
