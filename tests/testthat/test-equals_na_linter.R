context("equals_na_linter")

test_that("returns the correct linting", {
  msg <- rex("Use is.na for comparisons to NA (not == or !=)")
  expect_lint("blah", NULL, equals_na_linter)
  expect_lint("  blah", NULL, equals_na_linter)
  expect_lint("  blah", NULL, equals_na_linter)
  expect_lint("x=NA", NULL, equals_na_linter)

  expect_lint(
    "x == NA",
    list(message = msg, line_number = 1L, column_number = 3),
    equals_na_linter)

  expect_lint(
    "x==NA",
    list(message = msg, line_number = 1L, column_number = 2),
    equals_na_linter
  )

  expect_lint(
    "x==f(1, ignore = NA)",
    NULL,
    equals_na_linter
  )

 # equals_na_linter should ignore strings and comments
  expect_lint(
    "is.na(x) # dont flag x == NA if inside a comment",
    NULL,
    equals_na_linter
  )
  expect_lint(
    "msg <- 'dont flag x == NA if inside a string'",
    NULL,
    equals_na_linter
  )
})
