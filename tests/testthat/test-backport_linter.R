test_that("backport_linter produces error when R version misspecified", {
  expect_error(
    lint(text = "numToBits(2)", linters = backport_linter(420L)),
    "`r_version` must be an R version number"
  )
})

test_that("backport_linter detects backwards-incompatibility", {
  # default should be current R version; all of these are included on our dependency
  expect_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", NULL, backport_linter())
  expect_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", NULL, backport_linter("release"))
  expect_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", NULL, backport_linter("devel"))

  expect_lint(
    "numToBits(2)",
    rex::rex("numToBits (R 4.1.0) is not available for dependency R >= 4.0.0."),
    backport_linter("4.0.0")
  )
  # symbols as well as calls
  expect_lint(
    "lapply(1:10, numToBits)",
    rex::rex("numToBits (R 4.1.0) is not available for dependency R >= 4.0.0."),
    backport_linter("4.0.0")
  )

  expect_lint(
    trim_some("
      trimws(
        ...names()
      )
    "),
    list(
      list(rex::rex("trimws (R 3.2.0) is not available for dependency R >= 3.0.0."), line_number = 1L),
      list(rex::rex("...names (R 4.1.0) is not available for dependency R >= 3.0.0."), line_number = 2L)
    ),
    backport_linter("3.0.0")
  )

  # oldrel specification
  expect_lint(
    "grepv()",
    rex::rex("grepv (R 4.5.0) is not available for dependency R >= 4.4.3."),
    backport_linter("oldrel")
  )

  expect_error(
    backport_linter("oldrel-99"),
    "`r_version` is not valid"
  )

  expect_lint(
    "numToBits(2)",
    rex::rex("numToBits (R 4.1.0) is not available for dependency R >= 4.0.5."),
    backport_linter("oldrel-5")
  )
  # no interference from namespace-qualification (even of base functions)
  expect_lint(
    "base::numToBits(2)",
    rex::rex("numToBits (R 4.1.0) is not available for dependency R >= 4.0.5."),
    backport_linter("oldrel-5")
  )

  # except is honored
  expect_lint(
    trim_some("
      numToBits(2)
      R_user_dir('mypkg')
    "),
    NULL,
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
