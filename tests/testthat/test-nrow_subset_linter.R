test_that("nrow_subset_linter skips allowed usage", {
  linter <- nrow_subset_linter()

  expect_lint("nrow(foo(subset(x, y == z)))", NULL, linter)
  expect_lint("with(x, sum(y == z))", NULL, linter)
})

test_that("nrow_subset_linter blocks subset() cases", {
  expect_lint(
    "nrow(subset(x, y == z))",
    rex::rex("Use arithmetic to count the number of rows satisfying a condition"),
    nrow_subset_linter()
  )
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Use arithmetic to count the number of rows satisfying a condition")

  expect_lint(
    trim_some("{
      nrow(subset(x, y == z))
      subset(x) %>% transform(m = 2)
      nrow(subset(a, b == c))
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 4L)
    ),
    nrow_subset_linter()
  )
})
