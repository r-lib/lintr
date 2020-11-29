test_that("backport_linter detects backwards-incompatibility", {
  # this test may be too fragile?
  expect_lint("numToBits(1)", NULL, backport_linter("r-devel"))

  # default should be current R version; all of these are included on our dependency
  expect_lint(".getNamespaceInfo(dir.exists(lapply(x, toTitleCase)))", NULL, backport_linter())

  expect_lint(
    "numToBits(2)",
    rex("numToBits (R r-devel) is not available for dependency R >= 4.0.0."),
    backport_linter("4.0.0")
  )
  # symbols as well as calls
  expect_lint(
    "lapply(1:10, numToBits)",
    rex("numToBits (R r-devel) is not available for dependency R >= 4.0.0."),
    backport_linter("4.0.0")
  )

  expect_lint(
    "trimws(...names())",
    list(
      rex("trimws (R 3.2.0) is not available for dependency R >= 3.0.0."),
      rex("...names (R r-devel) is not available for dependency R >= 3.0.0.")
    ),
    backport_linter("3.0.0")
  )
})
