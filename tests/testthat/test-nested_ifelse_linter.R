test_that("nested_ifelse_linter skips allowed usages", {
  linter <- nested_ifelse_linter()

  expect_lint("if (TRUE) 1 else 2", NULL, linter)
  expect_lint("if (TRUE) 1 else if (TRUE) 2 else 3", NULL, linter)

  expect_lint("ifelse(runif(10) > .2, 4, 6)", NULL, linter)

  # don't block suggested alternatives
  expect_lint("fcase(l1, v1, l2, v2)", NULL, linter)
  expect_lint("case_when(l1 ~ v1, l2 ~ v2)", NULL, linter)
})

local({
  ifelse_calls <- c("ifelse", "if_else", "fifelse")

  linter <- nested_ifelse_linter()

  patrick::with_parameters_test_that(
    "nested_ifelse_linter blocks simple disallowed usages",
    {
      lint_msg <- rex::rex("Avoid nested ", ifelse_call, " calls")

      expect_lint(glue::glue("{ifelse_call}(l1, v1, {ifelse_call}(l2, v2, v3))"), lint_msg, linter)
      expect_lint(glue::glue("{ifelse_call}(l1, {ifelse_call}(l2, v1, v2), v3)"), lint_msg, linter)
    },
    ifelse_call = ifelse_calls
  )
})

test_that("nested_ifelse_linter also catches mixed usage", {
  # not sure why anyone would do this, but the readability still argument holds
  expect_lint(
    "data.table::fifelse(l1, dplyr::if_else(l2, v1, v2), v3)",
    rex::rex("Avoid nested if_else() calls"),
    nested_ifelse_linter()
  )
})
