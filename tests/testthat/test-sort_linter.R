test_that("sort_linter skips allowed usages", {
  expect_lint("order(y)", NULL, sort_linter())

  expect_lint("y[order(x)]", NULL, sort_linter())

  # If another function is intercalated, don't fail
  expect_lint("x[c(order(x))]", NULL, sort_linter())
})


test_that("sort_linter blocks simple disallowed usages", {
  lint_message <- rex::rex("sort(x) is better than x[order(x)].")

  expect_lint("x[order(x)]", lint_message, sort_linter())

  # Works with extra args in order()
  expect_lint("x[order(x, decreasing = TRUE)]", lint_message, sort_linter())

  # ...even in disorder
  expect_lint("x[order(decreasing = TRUE, x)]", lint_message, sort_linter())
})

test_that("sort_linter produces customized warning message", {

  expect_lint(
    "y[order(y)]",
    rex::rex("sort(y) is better than y[order(y)]."),
    sort_linter()
  )

})
