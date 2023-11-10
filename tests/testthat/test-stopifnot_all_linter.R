test_that("stopifnot_all_linter skips allowed usages", {
  expect_lint("stopifnot(all(x) || any(y))", NULL, stopifnot_all_linter())
})

test_that("stopifnot_all_linter blocks simple disallowed usages", {
  expect_lint(
    "stopifnot(all(A))",
    rex::rex("stopifnot(x) runs all() 'under the hood'"),
    stopifnot_all_linter()
  )

  expect_lint(
    "stopifnot(x, y, all(z))",
    rex::rex("stopifnot(x) runs all() 'under the hood'"),
    stopifnot_all_linter()
  )
})
