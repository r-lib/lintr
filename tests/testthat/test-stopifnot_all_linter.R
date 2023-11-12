test_that("stopifnot_all_linter skips allowed usages", {
  expect_lint("stopifnot(all(x) || any(y))", NULL, stopifnot_all_linter())
})

test_that("stopifnot_all_linter blocks simple disallowed usages", {
  expect_lint(
    "stopifnot(all(A))",
    list(rex::rex("stopifnot(x) runs all() 'under the hood'"), column_number = 11L),
    stopifnot_all_linter()
  )

  expect_lint(
    "stopifnot(x, y, all(z))",
    list(rex::rex("stopifnot(x) runs all() 'under the hood'"), column_number = 17L),
    stopifnot_all_linter()
  )
})
