context("equals_na_linter")

test_that("returns the correct linting", {
  msg <- rex("Use is.na rather than == NA.")
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

})
