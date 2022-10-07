test_that("equals_na_linter skips allowed usages", {
  linter <- equals_na_linter()

  expect_lint("blah", NULL, linter)
  expect_lint("  blah", NULL, linter)
  expect_lint("  blah", NULL, linter)
  expect_lint("x=NA", NULL, linter)
  expect_lint("x = NA_real_", NULL, linter)
  expect_lint("is.na(x)", NULL, linter)
  expect_lint("x[!is.na(x)]", NULL, linter)

  # equals_na_linter should ignore strings and comments
  expect_lint("is.na(x) # dont flag x == NA if inside a comment", NULL, linter)
  expect_lint("msg <- 'dont flag x == NA if inside a string'", NULL, linter)

  # nested NAs are okay
  expect_lint("x==f(1, ignore = NA)", NULL, linter)

})

test_that("equals_na_linter blocks disallowed usages", {
  linter <- equals_na_linter()
  msg <- rex::rex("Use is.na for comparisons to NA (not == or !=)")

  expect_lint(
    "x == NA",
    list(message = msg, line_number = 1L, column_number = 1L),
    linter
  )

  expect_lint(
    "x != NA",
    list(message = msg, line_number = 1L, column_number = 1L),
    linter
  )

  expect_lint(
    "x != NA_integer_",
    list(message = msg, line_number = 1L, column_number = 1L),
    linter
  )

  expect_lint(
    "x==NA",
    list(message = msg, line_number = 1L, column_number = 1L),
    linter
  )

  # correct line number for multiline code
  expect_lint(
    "x ==\nNA_character_",
    list(line_number = 1L, column_number = 1L, ranges = list(c(1L, 4L))),
    linter
  )
})
