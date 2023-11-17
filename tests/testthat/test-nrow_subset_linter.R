# TODO(michaelchirico): activate this false positive test when below cases are done.
# test_that("nrow_subset_linter skips allowed usages", {
#   # nrow can be avoided here (by chaining the expression and using .N),
#   #   but the benefit of doing so is not the same as in the other cases.
#   lintr::expect_lint(
#     "nrow(DT[x == y, 1, by = grp])",
#     NULL,
#     nrow_subset_linter
#   )
# })

test_that("nrow_subset_linter blocks subset() cases", {
  expect_lint(
    "nrow(subset(x, y == z))",
    rex::rex("Use arithmetic to count the number of rows satisfying a condition"),
    nrow_subset_linter()
  )

  # TODO(michaelchirico): implement this.
  # lintr::expect_lint(
  #   "x %>% subset(y == z) %>% nrow()",
  #   "Use arithmetic to count the number of rows satisfying a condition",
  #   nrow_subset_linter
  # )
})

# TODO(michaelchirico): implement these.
# test_that("nrow_subset_linter blocks [ cases", {
#   # data.frame subsetting (NB: replacement doesn't use na.rm = TRUE)
#   lintr::expect_lint(
#     "nrow(x[x$y == x$z, ])",
#     "Use arithmetic to count the number of rows satisfying a condition",
#     nrow_subset_linter
#   )

#   # data.table subsetting (NB: replacement needs na.rm = TRUE)
#   lintr::expect_lint(
#     "x[y == z, ]",
#     "Use arithmetic to count the number of rows satisfying a condition",
#     nrow_subset_linter
#   )
# })

# test_that("nrow_subset_linter blocks dplyr::filter() cases", {
#   lintr::expect_lint(
#     "x %>% filter(y == z) %>% nrow()",
#     "Use arithmetic to count the number of rows satisfying a condition",
#     nrow_subset_linter
#   )

#   lintr::expect_lint(
#     "nrow(dplyr::filter(x, y == z))",
#     "Use arithmetic to count the number of rows satisfying a condition",
#     nrow_subset_linter
#   )
# })
