test_that("returns the correct linting", {
  linter <- equals_na_linter()
  msg <- rex("Use is.na for comparisons to NA (not == or !=)")
  expect_lint("blah", NULL, linter)
  expect_lint("  blah", NULL, linter)
  expect_lint("  blah", NULL, linter)
  expect_lint("x=NA", NULL, linter)

  expect_lint(
    "x == NA",
    list(message = msg, line_number = 1L, column_number = 3),
    linter
  )

  expect_lint(
    "x==NA",
    list(message = msg, line_number = 1L, column_number = 2),
    linter
  )

  expect_lint(
    "x==f(1, ignore = NA)",
    NULL,
    linter
  )

 # equals_na_linter should ignore strings and comments
  expect_lint(
    "is.na(x) # dont flag x == NA if inside a comment",
    NULL,
    linter
  )
  expect_lint(
    "msg <- 'dont flag x == NA if inside a string'",
    NULL,
    linter
  )

  # correct line number for multiline code
  expect_lint(
    "x ==\nNA",
    list(line_number = 1L, column_number = 3L, ranges = list(3:4)),
    linter
  )
})
