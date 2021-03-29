test_that("returns the correct linting", {
  expect_lint(
    "stats::sd",
    NULL,
    namespace_linter()
  )

  expect_lint(
    "stats::sd(c(1,2,3))",
    NULL,
    namespace_linter()
  )

  expect_lint(
    "\"stats\"::sd(c(1,2,3))",
    NULL,
    namespace_linter()
  )

  expect_lint(
    "stats::\"sd\"(c(1,2,3))",
    NULL,
    namespace_linter()
  )

  expect_lint(
    "stats::`sd`(c(1,2,3))",
    NULL,
    namespace_linter()
  )

  expect_lint(
    "statts::sd(c(1,2,3))",
    list(message = rex("Package 'statts' is not installed.")),
    namespace_linter()
  )

  expect_lint(
    "stats::ssd(c(1,2,3))",
    list(message = rex("'ssd' is not exported from {stats}")),
    namespace_linter()
  )

  expect_lint(
    "stats::ssd(c(1,2,3))",
    NULL,
    namespace_linter(check_exports = FALSE)
  )

  expect_lint(
    "datasets::mtcars",
    NULL,
    namespace_linter()
  )

  expect_lint(
    "stats:::print.formula",
    NULL,
    namespace_linter()
  )

  expect_lint(
    "stats:::sd(c(1,2,3))",
    list(message = rex("'sd' is exported from {stats}. Use stats::sd instead.")),
    namespace_linter()
  )

  expect_lint(
    "statts:::sd(c(1,2,3))",
    list(message = rex("Package 'statts' is not installed.")),
    namespace_linter()
  )

  expect_lint(
    "stats:::sdd(c(1,2,3))",
    list(message = rex("'sdd' does not exist in {stats}")),
    namespace_linter()
  )

  expect_lint(
    "stats:::sdd(c(1,2,3))",
    NULL,
    namespace_linter(check_nonexports = FALSE)
  )

  expect_lint(
    "stats::sd(c(1,2,3))\nstats::sdd(c(1,2,3))",
    list(line = "stats::sdd(c(1,2,3))"),
    namespace_linter()
  )
})
