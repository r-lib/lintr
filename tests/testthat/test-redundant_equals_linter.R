test_that("redundant_equals_linter skips allowed usages", {
  # comparisons to non-logical constants
  expect_lint("x == 1", NULL, redundant_equals_linter())
  # comparison to TRUE as a string
  expect_lint("x != 'TRUE'", NULL, redundant_equals_linter())
})

test_that("redundant_equals_linter blocks simple disallowed usages", {
  linter <- redundant_equals_linter()
  eq_msg <- rex::rex("Using == on a logical vector is redundant.")
  ne_msg <- rex::rex("Using != on a logical vector is redundant.")
  expect_lint("x == TRUE", eq_msg, linter)
  expect_lint("x != TRUE", ne_msg, linter)
  expect_lint("x == FALSE", eq_msg, linter)
  expect_lint("x != FALSE", ne_msg, linter)

  # order doesn't matter
  expect_lint("TRUE == x", eq_msg, linter)
})

test_that("mutliple lints return correct custom messages", {
  expect_lint(
    "list(x == TRUE, y != TRUE)",
    list(
      "Using == on a logical vector",
      "Using != on a logical vector"
    ),
    redundant_equals_linter()
  )
})
