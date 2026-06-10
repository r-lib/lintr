test_that("backport_linter produces error when R version misspecified", {
  expect_error(
    lint(text = "numToBits(2)", linters = backport_linter(420L)),
    "`r_version` must be an R version number"
  )
})

test_that("backport_linter detects backwards-incompatibility", {
  # default should be current R version; all of these are included on our dependency
  expect_no_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", backport_linter())
  expect_no_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", backport_linter("release"))
  expect_no_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", backport_linter("devel"))
  expect_no_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", backport_linter("auto"))
  
  expect_lint(
    "numToBits(2)",
    rex::rex("numToBits (R 4.1.0) is not always available for requested dependency (R >= 4.0.0)."),
    backport_linter("4.0.0")
  )
  # symbols as well as calls
  expect_lint(
    "lapply(1:10, numToBits)",
    rex::rex("numToBits (R 4.1.0) is not always available for requested dependency (R >= 4.0.0)."),
    backport_linter("4.0.0")
  )

  expect_lint(
    trim_some("
      trimws(
        ...names()
      )
    "),
    list(
      list(rex::rex("trimws (R 3.2.0)", anything, "(R >= 3.0.0)."), line_number = 1L),
      list(rex::rex("...names (R 4.1.0)", anything, "(R >= 3.0.0)."), line_number = 2L)
    ),
    backport_linter("3.0.0")
  )

  # oldrel specification
  expect_lint(
    "grepv()",
    rex::rex("grepv (R 4.5.0) is not always available for requested dependency (R >= 4.4.3)."),
    backport_linter("oldrel")
  )

  expect_error(
    backport_linter("oldrel-99"),
    "`r_version` is not valid"
  )

  expect_lint(
    "numToBits(2)",
    rex::rex("numToBits (R 4.1.0) is not always available for requested dependency (R >= 4.0.5)."),
    backport_linter("oldrel-5")
  )
  # no interference from namespace-qualification (even of base functions)
  expect_lint(
    "base::numToBits(2)",
    rex::rex("numToBits (R 4.1.0) is not always available for requested dependency (R >= 4.0.5)."),
    backport_linter("oldrel-5")
  )

  # except is honored
  expect_no_lint(
    trim_some("
      numToBits(2)
      R_user_dir('mypkg')
    "),
    backport_linter("3.0.0", except = c("numToBits", "R_user_dir"))
  )
})

test_that("backport_linter generates expected warnings", {
  tmp <- withr::local_tempfile(lines = "x <- x + 1")

  expect_warning(
    {
      l <- lint(tmp, backport_linter("2.0.0"))
    },
    'version older than "3.0.0"',
    fixed = TRUE
  )
  expect_identical(l, lint(tmp, backport_linter("3.0.0")))
})

test_that("backport_linter works with package R version", {
  pkg_lints_from_outside <- lint_package(
    "dummy_packages/auto_backport_fail",
    linters = backport_linter(),
    parse_settings = FALSE
  )
  pkg_lints_from_root <- withr::with_dir(
    "dummy_packages/auto_backport_fail",
    lint_package(linters = backport_linter(), parse_settings = FALSE)
  )

  expect_length(pkg_lints_from_outside, 1L)
  expect_identical(
    pkg_lints_from_outside[[1]]$message,
    "numToBits (R 4.1.0) is not always available for requested dependency (R >= 4.0.0)."
  )
  expect_length(pkg_lints_from_root, 1L)
  expect_identical(
    pkg_lints_from_root[[1]]$message,
    "numToBits (R 4.1.0) is not always available for requested dependency (R >= 4.0.0)."
  )

  # Can be overwritten
  pkg_lints_from_outside_noauto <- lint_package(
    "dummy_packages/auto_backport_fail",    
    backport_linter("3.0.0"),
    parse_settings = FALSE
  )

  expect_length(pkg_lints_from_outside_noauto, 1L)
  expect_identical(
    pkg_lints_from_outside_noauto[[1]]$message,
    "numToBits (R 4.1.0) is not always available for requested dependency (R >= 3.0.0)."
  )

  pkg_lints_auto_pass <- lint_package(
    "dummy_packages/auto_backport_pass",
    linters = backport_linter(),
    parse_settings = FALSE
  )
  expect_length(pkg_lints_auto_pass, 0L)
})
