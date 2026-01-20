test_that("list_comparison_linter skips allowed usages", {
  expect_no_lint("sapply(x, sum) > 10", list_comparison_linter())
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

test_that("list_comparison_linter vectorizes", {
  expect_lint(
    trim_some("{
      sapply(x, sum) > 10
      .mapply(`+`, list(1:10, 1:10), NULL) == 2
      lapply(x, sum) < 5
    }"),
    list(
      list(rex::rex(".mapply()", anything, "`==`"), line_number = 3L),
      list(rex::rex("lapply()", anything, "`<`"), line_number = 4L)
    ),
    list_comparison_linter()
  )
})
