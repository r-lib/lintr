test_that("namespace_linter skips allowed usages", {
  linter <- namespace_linter()

  expect_no_lint("stats::sd", linter)
  expect_no_lint("stats::sd(c(1,2,3))", linter)
  expect_no_lint('"stats"::sd(c(1,2,3))', linter)
  expect_no_lint('stats::"sd"(c(1,2,3))', linter)
  expect_no_lint("stats::`sd`(c(1,2,3))", linter)

  expect_no_lint("datasets::mtcars", linter)
  expect_no_lint("stats:::print.formula", linter)
  expect_no_lint('"stats":::print.formula', linter)
})

test_that("namespace_linter respects check_exports and check_nonexports arguments", {
  expect_no_lint("stats::ssd(c(1,2,3))", namespace_linter(check_exports = FALSE))
  expect_no_lint("stats:::ssd(c(1,2,3))", namespace_linter(check_nonexports = FALSE))
  expect_no_lint("stats:::ssd(c(1,2,3))", namespace_linter(check_exports = FALSE, check_nonexports = FALSE))
})

test_that("namespace_linter can work with backticked symbols", {
  skip_if_not_installed("rlang")
  linter <- namespace_linter()

  expect_no_lint("rlang::`%||%`", linter)
  expect_no_lint("rlang::`%||%`()", linter)

  expect_no_lint("rlang::'%||%'", linter)
  expect_no_lint("rlang::'%||%'()", linter)
  expect_no_lint('rlang::"%||%"', linter)
  expect_no_lint('rlang::"%||%"()', linter)

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

test_that("namespace_linter detects functions already imported in the NAMESPACE", {
  pkg_dir <- withr::local_tempdir("testpkg")
  dir.create(file.path(pkg_dir, "R"))
  writeLines("Package: testpkg\nVersion: 1.0.0\n", file.path(pkg_dir, "DESCRIPTION"))
  writeLines("importFrom(stats, median)\nimportFrom(utils, head)\n", file.path(pkg_dir, "NAMESPACE"))

  linter <- namespace_linter()

  test_file <- file.path(pkg_dir, "R", "test.R")
  writeLines(
    trim_some("
      stats::median(1:10)
      utils:::head(1:10)
      stats::sd(1:10)
    "),
    test_file
  )

  expect_lint(
    content = "",
    file = test_file,
    list(
      list(rex::rex("Don't use `::` to access median, which is already imported from stats."), line_number = 1L),
      list(rex::rex("Don't use `:::` to access head, which is already imported from utils."), line_number = 2L)
    ),
    linter
  )

  test_file2 <- file.path(pkg_dir, "R", "test2.R")
  writeLines("stats::median(1:10)\n", test_file2)
  expect_no_lint(
    content = "",
    file = test_file2,
    namespace_linter(check_imports = FALSE)
  )
})

test_that("namespace_linter check_imports works with backticked symbols", {
  skip_if_not_installed("rlang")

  pkg_dir <- withr::local_tempdir("testpkg_rlang")
  dir.create(file.path(pkg_dir, "R"))
  writeLines("Package: testpkg_rlang\nVersion: 1.0.0\n", file.path(pkg_dir, "DESCRIPTION"))
  writeLines('importFrom("rlang", "%||%")\n', file.path(pkg_dir, "NAMESPACE"))

  linter <- namespace_linter()

  test_file <- file.path(pkg_dir, "R", "test.R")
  writeLines("rlang::`%||%`\n", test_file)

  expect_lint(
    content = "",
    file = test_file,
    rex::rex("Don't use `::` to access %||%, which is already imported from rlang."),
    linter
  )
})
