test_that("unused_import_linter lints as expected", {
  linter <- unused_import_linter()
  expect_lint("library(dplyr)\ntibble(a = 1)", NULL, linter)
  # SYMBOL_FUNCTION_CALL usage is detected
  expect_lint("library(tidyverse)\ntibble(a = 1)", NULL, linter)
  # SYMBOL usage is detected
  expect_lint("library(dplyr)\ndo.call(tibble, args = list(a = 1))", NULL, linter)
  # SPECIAL usage is detected
  expect_lint("library(magrittr)\n1:3 %>% mean()", NULL, linter)

  # Missing packages are ignored
  expect_lint("library(not.a.package)\ntibble(a = 1)", NULL, linter)
  # SYMBOL calls with character.only = TRUE are ignored, even if the argument is a package name
  expect_lint("library(dplyr, character.only = TRUE)\n1 + 1", NULL, linter)

  msg <- rex::rex("package 'dplyr' is attached but never used")

  expect_lint("library(dplyr)\n1 + 1", msg, linter)
  expect_lint("require(dplyr)\n1 + 1", msg, linter)
  expect_lint("library('dplyr')\n1 + 1", msg, linter)
  # ignore namespaced usages
  expect_lint("library(dplyr)\ndplyr::tibble(a = 1)", msg, linter)
})
