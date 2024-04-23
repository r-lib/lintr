test_that("missing_package_linter skips allowed usages", {
  linter <- missing_package_linter()

  expect_lint("library(stats)", NULL, linter)
  expect_lint('library("stats")', NULL, linter)
  expect_lint("library('stats')", NULL, linter)
  expect_lint("library(`stats`)", NULL, linter)
  expect_lint("library(stats, quietly)", NULL, linter)
  expect_lint("library(stats, quietly = TRUE)", NULL, linter)
  expect_lint("require(stats)", NULL, linter)
  expect_lint("require(stats, quietly = TRUE)", NULL, linter)
  expect_lint('loadNamespace("stats")', NULL, linter)
  expect_lint('requireNamespace("stats")', NULL, linter)
})

test_that("missing_package_linter blocks disallowed usages", {
  linter <- missing_package_linter()
  lint_msg <- rex::rex("Package 'statts' is not installed.")

  expect_lint("require(statts)", lint_msg, linter)
  expect_lint("library(statts, quietly = TRUE)", lint_msg, linter)
  expect_lint("library(statts, quietly = TRUE)", lint_msg, linter)
  expect_lint('loadNamespace("statts")', lint_msg, linter)
  expect_lint('requireNamespace("statts")', lint_msg, linter)

  expect_lint(
    trim_some("
      library(utils)
      library(statts)
    "),
    list(lint_msg, line_number = 2L, line = "library(statts)"),
    linter
  )
})

test_that("loadNamespace and requireNamespace allow plain symbols", {
  linter <- missing_package_linter()

  expect_lint("loadNamespace(mypkg)", NULL, linter)
  expect_lint("requireNamespace(mypkg)", NULL, linter)
})

test_that("character.only=TRUE case is handled", {
  linter <- missing_package_linter()

  expect_lint("library(statts, character.only = TRUE)", NULL, linter)
  expect_lint("require(statts, character.only = TRUE)", NULL, linter)
  expect_lint('library("stats", character.only = TRUE)', NULL, linter)

  expect_lint(
    'library("statts", character.only = TRUE)',
    rex::rex("Package 'statts' is not installed."),
    missing_package_linter()
  )
})
