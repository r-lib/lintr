test_that("nrow_subset_linter blocks subset() cases", {
  expect_lint(
    "nrow(subset(x, y == z))",
    rex::rex("Use arithmetic to count the number of rows satisfying a condition"),
    nrow_subset_linter()
  )
})
