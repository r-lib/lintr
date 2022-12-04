test_that("sort_linter skips allowed usages", {
  linter <- sort_linter()

  expect_lint("order(y)", NULL, linter)

  expect_lint("y[order(x)]", NULL, linter)

  # If another function is intercalated, don't fail
  expect_lint("x[c(order(x))]", NULL, linter)
})


test_that("sort_linter blocks simple disallowed usages", {
  linter <- sort_linter()
  lint_message <- rex::rex("sort(", anything, ") is better than")

  expect_lint("x[order(x)]", lint_message, linter)

  # Works with extra args in order()
  expect_lint("x[order(x, decreasing = TRUE)]", lint_message, linter)

  # ...even in disorder
  expect_lint("x[order(decreasing = TRUE, x)]", lint_message, linter)
})

test_that("sort_linter produces customized warning message", {
  linter <- sort_linter()

  expect_lint(
    "y[order(y)]",
    rex::rex("sort(y, na.last = TRUE) is better than y[order(y)]."),
    linter
  )

  # We capture the correct variable symbol
  expect_lint(
    "y + x[order(x)]",
    rex::rex("sort(x, na.last = TRUE) is better than x[order(x)]."),
    linter
  )

  # Default na.last = TRUE is overwritten if na.last is already provided
  expect_lint(
    "x[order(x, na.last = FALSE)]",
    rex::rex("sort(x, na.last = FALSE) is better than x[order(x, na.last = FALSE)]."),
    linter
  )

  expect_lint(
    "x[order(x, decreasing = FALSE)]",
    rex::rex("sort(x, decreasing = FALSE, na.last = TRUE) is better than x[order(x, decreasing = FALSE)]."),
    linter
  )

  expect_lint(
    "f()[order(f())]",
    rex::rex("sort(f(), na.last = TRUE) is better than f()[order(f())]"),
    linter
  )
})

test_that("sort_linter works with multiple lints in a single expression", {
  linter <- sort_linter()

  expect_lint(
    "c(
      x[order(x)],
      y[order(y, decreasing = TRUE, na.last = FALSE)]
    )",
    list(
      rex::rex("sort(x, na.last = TRUE) is better than x[order(x)]."),
      rex::rex(
        "sort(y, decreasing = TRUE, na.last = FALSE)",
        anything,
        "y[order(y, decreasing = TRUE, na.last = FALSE)]."
      )
    ),
    linter
  )

})
