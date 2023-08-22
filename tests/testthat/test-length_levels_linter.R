test_that("length_levels_linter skips allowed usages", {
  expect_lint("length(c(levels(x), 'a'))", NULL, length_levels_linter())
})

test_that("length_levels_linter blocks simple disallowed usages", {
  expect_lint(
    "2:length(levels(x))",
    rex::rex("nlevels(x) is better than length(levels(x))."),
    length_levels_linter()
  )
})
