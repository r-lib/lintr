test_that("returns the correct linting", {
  linter <- namespace_linter()

  expect_lint(
    "stats::sd",
    NULL,
    linter
  )

  expect_lint(
    "stats::sd(c(1,2,3))",
    NULL,
    linter
  )

  expect_lint(
    '"stats"::sd(c(1,2,3))',
    NULL,
    linter
  )

  expect_lint(
    'stats::"sd"(c(1,2,3))',
    NULL,
    linter
  )

  expect_lint(
    "stats::`sd`(c(1,2,3))",
    NULL,
    linter
  )

  expect_lint(
    "statts::sd(c(1,2,3))",
    list(message = rex::rex("Package 'statts' is not installed.")),
    linter
  )

  expect_lint(
    "stats::ssd(c(1,2,3))",
    list(message = rex::rex("'ssd' is not exported from {stats}")),
    linter
  )

  expect_lint(
    "stats::ssd(c(1,2,3))",
    NULL,
    namespace_linter(check_exports = FALSE)
  )

  expect_lint(
    "datasets::mtcars",
    NULL,
    linter
  )

  expect_lint(
    "stats:::print.formula",
    NULL,
    linter
  )

  expect_lint(
    "stats:::sd(c(1,2,3))",
    list(message = rex::rex("'sd' is exported from {stats}. Use stats::sd instead.")),
    linter
  )

  expect_lint(
    "statts:::sd(c(1,2,3))",
    list(message = rex::rex("Package 'statts' is not installed.")),
    linter
  )

  expect_lint(
    "stats:::sdd(c(1,2,3))",
    list(message = rex::rex("'sdd' does not exist in {stats}")),
    linter
  )

  expect_lint(
    "stats:::sdd(c(1,2,3))",
    NULL,
    namespace_linter(check_nonexports = FALSE)
  )

  expect_lint(
    "stats::sd(c(1,2,3))\nstats::sdd(c(1,2,3))",
    list(line = "stats::sdd(c(1,2,3))"),
    linter
  )
})
