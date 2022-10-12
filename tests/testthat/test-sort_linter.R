test_that("sort_linter skips allowed usages", {
  expect_lint("order(y)", NULL, sort_linter())

  expect_lint("y[order(x)]", NULL, sort_linter())

  # If another function is intercalated, don't fail
  expect_lint("x[c(order(x))]", NULL, sort_linter())
})


test_that("sort_linter blocks simple disallowed usages", {
  lint_message <- rex::rex("sort(", anything, ") is better than")

  expect_lint("x[order(x)]", lint_message, sort_linter())

  # Works with extra args in order()
  expect_lint("x[order(x, decreasing = TRUE)]", lint_message, sort_linter())

  # ...even in disorder
  expect_lint("x[order(decreasing = TRUE, x)]", lint_message, sort_linter())
})

test_that("sort_linter produces customized warning message", {

  expect_lint(
    "y[order(y)]",
    rex::rex("sort(y, na.last = TRUE) is better than y[order(y)]."),
    sort_linter()
  )

  # We capture the correct variable symbol
  expect_lint(
    "y + x[order(x)]",
    rex::rex("sort(x, na.last = TRUE) is better than x[order(x)]."),
    sort_linter()
  )

  # Default na.last = TRUE is overwritten if na.last is already provided
  expect_lint(
    "x[order(x, na.last = FALSE)]",
    rex::rex("sort(x, na.last = FALSE) is better than x[order(x, na.last = FALSE)]."),
    sort_linter()
  )

  expect_lint(
    "x[order(x, decreasing = FALSE)]",
    rex::rex("sort(x, decreasing = FALSE, na.last = TRUE) is better than x[order(x, decreasing = FALSE)]."),
    sort_linter()
  )


})
