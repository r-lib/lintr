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

test_that("nrow_subset_linter blocks filter() cases", {
  expect_lint(
    "nrow(filter(x, y == z))",
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
      x %>% filter(y == z) %>% nrow()
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 4L),
      list(lint_msg, line_number = 5L)
    ),
    nrow_subset_linter()
  )
})

test_that("linter is pipeline-aware", {
  linter <- nrow_subset_linter()
  lint_msg <- "Use arithmetic to count the number of rows satisfying a condition"

  expect_lint("x %>% subset(y == z) %>% nrow()", lint_msg, linter)
  expect_lint("x |> subset(y == z) |> nrow()", lint_msg, linter)
  expect_lint("filter(x, y == z) %>% nrow()", lint_msg, linter)
})
