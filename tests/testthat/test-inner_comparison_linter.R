test_that("inner_comparison_linter skips allowed usages", {
  # lapply returns a list, so not the same, though as.list is probably
  #   a better choice
  expect_lint(
    "lapply(x, function(xi) foo(xi) == 2)",
    NULL,
    inner_comparison_linter()
  )

  # this _may_ return a matrix, though outer is probably a better choice if so
  expect_lint(
    "sapply(x, function(xi) foo(xi) == y)",
    NULL,
    inner_comparison_linter()
  )
})

test_that("inner_comparison_linter blocks simple disallowed usages", {
  expect_lint(
    "sapply(x, function(xi) foo(xi) == 2)",
    R"[Compare to a constant after calling sapply\(\)/vapply\(\)]",
    inner_comparison_linter()
  )
  expect_lint(
    "sapply(x, function(xi) foo(xi) == 'a')",
    R"[Compare to a constant after calling sapply\(\)/vapply\(\)]",
    inner_comparison_linter()
  )
  expect_lint(
    "sapply(x, function(xi) foo(xi) == 1 + 2i)",
    R"[Compare to a constant after calling sapply\(\)/vapply\(\)]",
    inner_comparison_linter()
  )

  # vapply counts as well
  # NB: we ignore the FUN.VALUE argument, for now
  expect_lint(
    "vapply(x, function(xi) foo(xi) == 2, logical(1L))",
    R"[Compare to a constant after calling sapply\(\)/vapply\(\)]",
    inner_comparison_linter()
  )
})

test_that("inner_comparison_linter blocks other comparators as well", {
  expect_lint(
    "sapply(x, function(xi) foo(xi) >= 2)",
    R"[Compare to a constant after calling sapply\(\)/vapply\(\)]",
    inner_comparison_linter()
  )
  expect_lint(
    "sapply(x, function(xi) foo(xi) != 'a')",
    R"[Compare to a constant after calling sapply\(\)/vapply\(\)]",
    inner_comparison_linter()
  )
  expect_lint(
    "sapply(x, function(xi) foo(xi) < 1 + 2i)",
    R"[Compare to a constant after calling sapply\(\)/vapply\(\)]",
    inner_comparison_linter()
  )
})
