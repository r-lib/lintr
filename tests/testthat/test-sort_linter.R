test_that("sort_linter skips allowed usages", {
  linter <- sort_linter()

  expect_lint("order(y)", NULL, linter)

  expect_lint("y[order(x)]", NULL, linter)

  # If another function is intercalated, don't fail
  expect_lint("x[c(order(x))]", NULL, linter)

  expect_lint("x[order(y, x)]", NULL, linter)
  expect_lint("x[order(x, y)]", NULL, linter)
  # pretty sure this never makes sense, but test anyway
  expect_lint("x[order(y, na.last = x)]", NULL, linter)
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

test_that("sort_linter skips usages calling sort arguments", {
  linter <- sort_linter()

  # any arguments to sort --> not compatible
  expect_lint("sort(x, decreasing = TRUE) == x", NULL, linter)
  expect_lint("sort(x, na.last = TRUE) != x", NULL, linter)
  expect_lint("sort(x, method_arg = TRUE) == x", NULL, linter)
})

test_that("sort_linter skips when inputs don't match", {
  linter <- sort_linter()

  expect_lint("sort(x) == y", NULL, linter)
  expect_lint("sort(x) == foo(x)", NULL, linter)
  expect_lint("sort(foo(x)) == x", NULL, linter)
})

test_that("sort_linter blocks simple disallowed usages", {
  linter <- sort_linter()
  unsorted_msg <- rex::rex("Use is.unsorted(x) to test the unsortedness of a vector.")
  sorted_msg <- rex::rex("Use !is.unsorted(x) to test the sortedness of a vector.")

  expect_lint("sort(x) == x", sorted_msg, linter)
  expect_lint("identical(x, sort(x))", sorted_msg, linter)

  # argument order doesn't matter
  expect_lint("x == sort(x)", sorted_msg, linter)
  expect_lint("identical(sort(x), x)", sorted_msg, linter)

  # inverted version
  expect_lint("sort(x) != x", unsorted_msg, linter)
  expect_lint("!identical(x, sort(x))", unsorted_msg, linter)

  # expression matching
  expect_lint("sort(foo(x)) == foo(x)", sorted_msg, linter)
  expect_lint("identical(foo(x), sort(foo(x))", sorted_msg, linter)
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      x == sort(x)
      y[order(y)]
    }"),
    list(
      list(rex::rex("is.unsorted(x)"), line_number = 2L),
      list(rex::rex("sort(y"), line_number = 3L)
    ),
    sort_linter()
  )
})
