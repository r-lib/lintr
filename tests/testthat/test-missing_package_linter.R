context("missing_package_linter")
test_that("returns the correct linting", {
  expect_lint("library(stats)",
    NULL,
    missing_package_linter)

  expect_lint("library(\"stats\")",
    NULL,
    missing_package_linter)

  expect_lint("library('stats')",
    NULL,
    missing_package_linter)

  expect_lint("library(`stats`)",
    NULL,
    missing_package_linter)

  expect_lint("library(stats, quietly)",
    NULL,
    missing_package_linter)

  expect_lint("library(stats, quietly = TRUE)",
    NULL,
    missing_package_linter)

  expect_lint("library(statts, quietly = TRUE)",
    list(message = rex("Package 'statts' is not installed.")),
    missing_package_linter)

  expect_lint("library(statts, quietly = TRUE)",
    list(message = rex("Package 'statts' is not installed.")),
    missing_package_linter)

  expect_lint("require(stats)",
    NULL,
    missing_package_linter)

  expect_lint("require(stats, quietly = TRUE)",
    NULL,
    missing_package_linter)

  expect_lint("require(statts)",
    list(message = rex("Package 'statts' is not installed.")),
    missing_package_linter)

  expect_lint("loadNamespace(\"stats\")",
    NULL,
    missing_package_linter)

  expect_lint("loadNamespace(\"statts\")",
    list(message = rex("Package 'statts' is not installed.")),
    missing_package_linter)

  expect_lint("requireNamespace(\"stats\")",
    NULL,
    missing_package_linter)

  expect_lint("requireNamespace(\"statts\")",
    list(message = rex("Package 'statts' is not installed.")),
    missing_package_linter)
})
