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

  # Setter calls check `foo<-` rather than `foo`
  expect_no_lint("base::.rowNamesDF(df) <- c('x', 'y')", linter)
  expect_no_lint("base::`.rowNamesDF`(df) <- c('x', 'y')", linter)
  expect_no_lint('base::".rowNamesDF"(df) <- c("x", "y")', linter)
  expect_lint("base::.rowNamesDF(df)", "'.rowNamesDF' is not exported from {base}.", linter)
  expect_lint("base::mean(df) <- 1", "'mean<-' is not exported from {base}.", linter)
  expect_lint("1 -> base::mean(df)", "'mean<-' is not exported from {base}.", linter)
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

test_that("namespace_linter detects functions already imported in the NAMESPACE", { # nofuzz
  pkg_dir <- withr::local_tempdir("testpkg")
  dir.create(file.path(pkg_dir, "R"))
  write.dcf(
    list(Package = "testpkg", Version = "1.0.0"),
    file.path(pkg_dir, "DESCRIPTION")
  )
  writeLines(
    c("importFrom(stats, median)", "importFrom(utils, head)"),
    file.path(pkg_dir, "NAMESPACE")
  )

  test_file <- file.path(pkg_dir, "R", "test.R")
  writeLines(
    c(
      "stats::median(1:10)",
      "utils:::head(1:10)",
      "stats::sd(1:10) # not imported"
    ),
    test_file
  )

  expect_lint(
    file = test_file,
    checks = list(
      list("Don't use `::` to access median.*already imported", line_number = 1L),
      list("Don't use `:::` to access head.*already imported", line_number = 2L)
    ),
    linters = namespace_linter(check_nonexports = FALSE)
  )
})

test_that("namespace_linter works with backticked symbols", { # nofuzz
  skip_if_not_installed("rlang")

  pkg_dir <- withr::local_tempdir("testpkg_rlang")
  dir.create(file.path(pkg_dir, "R"))
  write.dcf(
    list(Package = "testpkg_rlang", Version = "1.0.0"),
    file.path(pkg_dir, "DESCRIPTION")
  )
  writeLines('importFrom("rlang", "%||%")', file.path(pkg_dir, "NAMESPACE"))

  test_file <- file.path(pkg_dir, "R", "test.R")
  writeLines("rlang::`%||%`", test_file)

  expect_lint(
    file = test_file,
    checks = rex::rex("Don't use `::` to access %||%, which is already imported from rlang."),
    linters = namespace_linter()
  )
})

test_that("namespace_linter respects except= in import() directives", { # nofuzz
  pkg_dir <- withr::local_tempdir("testpkg_except")
  dir.create(file.path(pkg_dir, "R"))
  writeLines(
    c("Package: testpkg_except", "Version: 1.0.0", "Title: Test", "Description: Test."),
    file.path(pkg_dir, "DESCRIPTION")
  )
  writeLines("import(stats, except = sd)", file.path(pkg_dir, "NAMESPACE"))

  test_file <- file.path(pkg_dir, "R", "test.R")
  writeLines(
    c(
      "stats::median(1:10)",
      "stats::sd(1:10) # excluded from the import"
    ),
    test_file
  )

  expect_lint(
    file = test_file,
    checks = list(rex::rex("Don't use `::` to access median, which is already imported from stats."), line_number = 1L),
    linters = namespace_linter()
  )
})

test_that("namespace_linter includes problematic package names in error message (#1855)", { # nofuzz
  lib_dir <- withr::local_tempdir("broken_lib")
  dir.create(file.path(lib_dir, "brokenpkg1", "Meta"), recursive = TRUE)
  file.create(file.path(lib_dir, "brokenpkg1", "Meta", "package.rds"))
  dir.create(file.path(lib_dir, "brokenpkg2", "Meta"), recursive = TRUE)
  file.create(file.path(lib_dir, "brokenpkg2", "Meta", "package.rds"))
  withr::local_libpaths(lib_dir, action = "prefix")

  expect_error(
    lint(text = "brokenpkg1::foo(); brokenpkg2:::bar()", linters = namespace_linter()),
    rex::rex("Failed to retrieve namespaces for", anything, "brokenpkg1", anything, "brokenpkg2")
  )
})

test_that("namespace_linter is OS-independent for base R packages (#3075)", {
  linter <- namespace_linter()

  # Windows-only exports
  expect_no_lint("base::shell('ls')", linter)
  expect_no_lint("base::shell.exec('file.txt')", linter)
  expect_no_lint("base::Sys.junction('from', 'to')", linter)
  expect_no_lint("base::.fixupGFortranStdout()", linter)
  expect_no_lint("grDevices::win.metafile()", linter)
  expect_no_lint("grDevices::windows()", linter)
  expect_no_lint("utils::winProgressBar()", linter)
  expect_no_lint("utils::readClipboard()", linter)
  expect_no_lint("utils::shortPathName('path')", linter)

  # Unix-only exports
  expect_no_lint("grDevices::X11.options()", linter)
  expect_no_lint("grDevices::quartz()", linter)
  expect_no_lint("parallel::mcparallel(1 + 1)", linter)
  expect_no_lint("parallel::mccollect()", linter)
  expect_no_lint("utils::nsl('localhost')", linter)

  # OS-specific unexported symbols
  expect_no_lint("utils:::unpackPkgZip()", linter)
  expect_no_lint("parallel:::mcfork()", linter)

  # Accessing OS-specific exports with `:::` is still linted consistently across OSes
  expect_lint(
    "base:::.fixupGFortranStdout()",
    list(
      rex::rex("Don't use `:::` to access .fixupGFortranStdout, which is exported from base."),
      line_number = 1L,
      column_number = 8L
    ),
    linter
  )
  expect_lint(
    "grDevices:::win.metafile()",
    list(
      rex::rex("Don't use `:::` to access win.metafile, which is exported from grDevices."),
      line_number = 1L,
      column_number = 13L
    ),
    linter
  )
})
