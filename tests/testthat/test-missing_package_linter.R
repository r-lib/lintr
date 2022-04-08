test_that("returns the correct linting", {
  linter <- missing_package_linter()
  msg <- list(message = rex("Package 'statts' is not installed."))

  expect_lint("library(stats)", NULL, linter)
  expect_lint('library("stats")', NULL, linter)
  expect_lint("library('stats')", NULL, linter)
  expect_lint("library(`stats`)", NULL, linter)
  expect_lint("library(stats, quietly)", NULL, linter)
  expect_lint("library(stats, quietly = TRUE)", NULL, linter)

  expect_lint("library(statts, quietly = TRUE)", msg, linter)

  expect_lint(
    trim_some("
      library(utils)
      library(statts)
    "),
    list(line = "library(statts)"),
    linter
  )

  expect_lint("library(statts, quietly = TRUE)", msg, linter)

  expect_lint("require(stats)", NULL, linter)
  expect_lint("require(stats, quietly = TRUE)", NULL, linter)
  expect_lint("require(statts)", msg, linter)

  expect_lint('loadNamespace("stats")', NULL, linter)
  expect_lint('loadNamespace("statts")', msg, linter)

  expect_lint('requireNamespace("stats")', NULL, linter)
  expect_lint('requireNamespace("statts")', msg, linter)
})

test_that("loadNamespace and requireNamespace allow plain symbols", {
  expect_lint("loadNamespace(mypkg)", NULL, missing_package_linter())
  expect_lint("requireNamespace(mypkg)", NULL, missing_package_linter())
})

test_that("character.only=TRUE case is handled", {
  expect_lint("library(statts, character.only = TRUE)", NULL, missing_package_linter())
  expect_lint("require(statts, character.only = TRUE)", NULL, missing_package_linter())

  expect_lint('library("stats", character.only = TRUE)', NULL, missing_package_linter())
  expect_lint(
    'library("statts", character.only = TRUE)',
    rex::rex("Package 'statts' is not installed."),
    missing_package_linter()
  )
})
