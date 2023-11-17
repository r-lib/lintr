test_that("inner_comparison_linter skips allowed usages", {
  linter <- inner_comparison_linter()

  # lapply returns a list, so not the same, though as.list is probably
  #   a better choice
  expect_lint("lapply(x, function(xi) foo(xi) == 2)", NULL, linter)

  # this _may_ return a matrix, though outer is probably a better choice if so
  expect_lint("sapply(x, function(xi) foo(xi) == y)", NULL, linter)
})

test_that("inner_comparison_linter blocks simple disallowed usages", {
  linter <- inner_comparison_linter()
  lint_msg <- rex::rex("Compare to a constant after calling sapply()/vapply()")

  expect_lint("sapply(x, function(xi) foo(xi) == 2)", lint_msg, linter)
  expect_lint("sapply(x, function(xi) foo(xi) == 'a')", lint_msg, linter)
  expect_lint("sapply(x, function(xi) foo(xi) == 1 + 2i)", lint_msg, linter)

  # vapply counts as well
  # NB: we ignore the FUN.VALUE argument, for now
  expect_lint("vapply(x, function(xi) foo(xi) == 2, logical(1L))", lint_msg, linter)
})

test_that("inner_comparison_linter blocks other comparators as well", {
  linter <- inner_comparison_linter()
  lint_msg <- rex::rex("Compare to a constant after calling sapply()/vapply()")

  expect_lint("sapply(x, function(xi) foo(xi) >= 2)", lint_msg, linter)
  expect_lint("sapply(x, function(xi) foo(xi) != 'a')", lint_msg, linter)
  expect_lint("sapply(x, function(xi) foo(xi) < 1 + 2i)", lint_msg, linter)
})
