test_that("namespace_linter skips allowed usages", {
  linter <- namespace_linter()

  expect_lint("stats::sd", NULL, linter)
  expect_lint("stats::sd(c(1,2,3))", NULL, linter)
  expect_lint('"stats"::sd(c(1,2,3))', NULL, linter)
  expect_lint('stats::"sd"(c(1,2,3))', NULL, linter)
  expect_lint("stats::`sd`(c(1,2,3))", NULL, linter)

  expect_lint("datasets::mtcars", NULL, linter)
  expect_lint("stats:::print.formula", NULL, linter)
  expect_lint('"stats":::print.formula', NULL, linter)
})

test_that("namespace_linter respects check_exports and check_nonexports arguments", {
  expect_lint("stats::ssd(c(1,2,3))", NULL, namespace_linter(check_exports = FALSE))
  expect_lint("stats:::ssd(c(1,2,3))", NULL, namespace_linter(check_nonexports = FALSE))
  expect_lint("stats:::ssd(c(1,2,3))", NULL, namespace_linter(check_exports = FALSE, check_nonexports = FALSE))
})

test_that("namespace_linter can work with backticked symbols", {
  skip_if_not_installed("rlang")
  linter <- namespace_linter()

  expect_lint("rlang::`%||%`", NULL, linter)
  expect_lint("rlang::`%||%`()", NULL, linter)

  expect_lint("rlang::'%||%'", NULL, linter)
  expect_lint("rlang::'%||%'()", NULL, linter)
  expect_lint('rlang::"%||%"', NULL, linter)
  expect_lint('rlang::"%||%"()', NULL, linter)

  expect_lint("rlang::`%>%`", "'%>%' is not exported from {rlang}.", linter)
  expect_lint("rlang::'%>%'()", "'%>%' is not exported from {rlang}.", linter)
  expect_lint('rlang::"%>%"()', "'%>%' is not exported from {rlang}.", linter)
})

test_that("namespace_linter blocks disallowed usages", {
  linter <- namespace_linter()

  expect_lint(
    "statts::sd(c(1,2,3))",
    rex::rex("Package 'statts' is not installed."),
    linter
  )

  expect_lint(
    "stats::ssd(c(1,2,3))",
    rex::rex("'ssd' is not exported from {stats}"),
    linter
  )

  expect_lint(
    "stats:::sd(c(1,2,3))",
    rex::rex("Don't use `:::` to access sd, which is exported from stats."),
    linter
  )

  expect_lint(
    "statts:::sd(c(1,2,3))",
    rex::rex("Package 'statts' is not installed."),
    linter
  )

  expect_lint(
    "stats:::sdd(c(1,2,3))",
    rex::rex("'sdd' does not exist in {stats}"),
    linter
  )

  expect_lint(
    trim_some("
      stats::sd(c(1,2,3))
      stats::sdd(c(1,2,3))
    "),
    list(line = "stats::sdd(c(1,2,3))"),
    linter
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      statts::sd(c(1,2,3))
      stats::ssd(c(1,2,3))
      stats:::sd(c(1,2,3))
    }"),
    list(
      list(rex::rex("Package 'statts' is not installed."), line_number = 2L),
      list(rex::rex("'ssd' is not exported from {stats}"), line_number = 3L),
      list(rex::rex("Don't use `:::` to access sd"), line_number = 4L)
    ),
    namespace_linter()
  )
})
