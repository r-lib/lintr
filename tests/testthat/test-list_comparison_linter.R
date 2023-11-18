test_that("list_comparison_linter skips allowed usages", {
  expect_lint("sapply(x, sum) > 10", NULL, list_comparison_linter())
})

local({
  linter <- list_comparison_linter()
  lint_msg <- rex::rex("a list(), is being coerced for comparison")


  cases <- expand.grid(
    list_mapper = c("lapply", "map", "Map", ".mapply"),
    comparator = c("==", "!=", ">=", "<=", ">", "<")
  )
  cases$.test_name <- with(cases, paste(list_mapper, comparator))
  patrick::with_parameters_test_that(
    "list_comparison_linter blocks simple disallowed usages",
    expect_lint(sprintf("%s(x, sum) %s 10", list_mapper, comparator), lint_msg, linter),
    .cases = cases
  )
})
