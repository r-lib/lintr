test_that("equals_na_linter skips allowed usages", {
  linter <- equals_na_linter()

  expect_no_lint("blah", linter)
  expect_no_lint("  blah", linter)
  expect_no_lint("  blah", linter)
  expect_no_lint("x=NA", linter)
  expect_no_lint("x = NaN", linter)
  expect_no_lint("x = NA_real_", linter)
  expect_no_lint("is.na(x)", linter)
  expect_no_lint("is.nan(x)", linter)
  expect_no_lint("x[!is.na(x)]", linter)

  # equals_na_linter should ignore strings and comments
  expect_no_lint("is.na(x) # do not flag x == NA if inside a comment", linter)
  expect_no_lint("lint_msg <- 'do not flag x == NA if inside a string'", linter)

  # nested NAs are okay
  expect_no_lint("x==f(1, ignore = NA)", linter)

  # this should be covered by any_is_na_linter()
  expect_no_lint("NA %in% x", linter)
})

skip_if_not_installed("tibble")
patrick::with_parameters_test_that(
  "equals_na_linter blocks disallowed usages for all combinations of operators and types of NAs",
  expect_lint(
    paste("x", operation, type_na),
    rex::rex("Use is.na() instead of x ", operation, " NA"),
    equals_na_linter()
  ),
  .cases = tibble::tribble(
    ~.test_name,                 ~operation, ~type_na,
    "equality, logical NA",      "==",       "NA",
    "equality, integer NA",      "==",       "NA_integer_",
    "equality, real NA",         "==",       "NA_real_",
    "equality, complex NA",      "==",       "NA_complex_",
    "equality, character NA",    "==",       "NA_character_",
    "containment, logical NA",   "%in%",     "NA",
    "containment, integer NA",   "%in%",     "NA_integer_",
    "containment, real NA",      "%in%",     "NA_real_",
    "containment, complex NA",   "%in%",     "NA_complex_",
    "containment, character NA", "%in%",     "NA_character_",
    "inequality, logical NA",    "!=",       "NA",
    "inequality, integer NA",    "!=",       "NA_integer_",
    "inequality, real NA",       "!=",       "NA_real_",
    "inequality, complex NA",    "!=",       "NA_complex_",
    "inequality, character NA",  "!=",       "NA_character_"
  )
)

test_that("equals_na_linter blocks disallowed usages in edge cases", {
  linter <- equals_na_linter()
  lint_msg_part <- "Use is.na() instead of x "

  # missing spaces around operators
  expect_lint(
    "x==NA",
    list(message = rex::rex(lint_msg_part, "== NA"), line_number = 1L, column_number = 1L),
    linter
  )
  expect_lint(
    "x!=NA",
    list(message = rex::rex(lint_msg_part, "!= NA"), line_number = 1L, column_number = 1L),
    linter
  )

  # order doesn't matter
  expect_lint(
    "NA == x",
    list(message = rex::rex(lint_msg_part, "== NA"), line_number = 1L, column_number = 1L),
    linter
  )

  # correct line number for multiline code
  expect_lint(
    "x ==\nNA",
    list(line_number = 1L, column_number = 1L, ranges = list(c(1L, 4L))),
    linter
  )
})
