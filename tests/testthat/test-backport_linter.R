test_that("backport_linter detects backwards-incompatibility", {
  # default should be current R version; all of these are included on our dependency
  expect_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", NULL, backport_linter())
  expect_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", NULL, backport_linter("release"))
  expect_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", NULL, backport_linter("devel"))

  # don't allow dependencies older than we've recorded
  writeLines("x <- x + 1", tmp <- tempfile())
  on.exit(unlink(tmp))

  expect_warning(l <- lint(tmp, backport_linter("2.0.0")), "version older than 3.0.0", fixed = TRUE)
  expect_identical(l, lint(tmp, backport_linter("3.0.0")))

  expect_lint(
    "numToBits(2)",
    rex("numToBits (R 4.1.0) is not available for dependency R >= 4.0.0."),
    backport_linter("4.0.0")
  )
  # symbols as well as calls
  expect_lint(
    "lapply(1:10, numToBits)",
    rex("numToBits (R 4.1.0) is not available for dependency R >= 4.0.0."),
    backport_linter("4.0.0")
  )

  expect_lint(
    "trimws(...names())",
    list(
      rex("trimws (R 3.2.0) is not available for dependency R >= 3.0.0."),
      rex("...names (R 4.1.0) is not available for dependency R >= 3.0.0.")
    ),
    backport_linter("3.0.0")
  )

  # oldrel specification
  expect_lint(
    "numToBits(2)",
    rex("numToBits (R 4.1.0) is not available for dependency R >= 4.0.0."),
    backport_linter("oldrel")
  )

  # NB: oldrel-1 could be 3.6.3, but we don't have any backports listed for 3.6.3
  expect_lint(
    "numToBits(2)",
    rex("numToBits (R 4.1.0) is not available for dependency R >= 3.6.0."),
    backport_linter("oldrel-1")
  )
})
