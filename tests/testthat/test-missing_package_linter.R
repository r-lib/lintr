test_that("returns the correct linting", {
  linter <- missing_package_linter()
  msg <- list(message = rex("Package 'statts' is not installed."))

  expect_lint("library(stats)", NULL, linter)
  expect_lint("library(\"stats\")", NULL, linter)
  expect_lint("library('stats')", NULL, linter)
  expect_lint("library(`stats`)", NULL, linter)
  expect_lint("library(stats, quietly)", NULL, linter)
  expect_lint("library(stats, quietly = TRUE)", NULL, linter)

  expect_lint("library(statts, quietly = TRUE)", msg, linter)

  expect_lint(
    "library(utils)\nlibrary(statts)\n",
    list(line = "library(statts)"),
    linter
  )

  expect_lint("library(statts, quietly = TRUE)", msg, linter)

  expect_lint("require(stats)", NULL, linter)
  expect_lint("require(stats, quietly = TRUE)", NULL, linter)
  expect_lint("require(statts)", msg, linter)

  expect_lint("loadNamespace(\"stats\")", NULL, linter)
  expect_lint("loadNamespace(\"statts\")", msg, linter)

  expect_lint("requireNamespace(\"stats\")", NULL, linter)
  expect_lint("requireNamespace(\"statts\")", msg, linter)
})
