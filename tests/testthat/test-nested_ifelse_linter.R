test_that("nested_ifelse_linter skips allowed usages", {
  linter <- nested_ifelse_linter()

  expect_no_lint("if (TRUE) 1 else 2", linter)
  expect_no_lint("if (TRUE) 1 else if (TRUE) 2 else 3", linter)

  expect_no_lint("ifelse(runif(10) > .2, 4, 6)", linter)

  # don't block suggested alternatives
  expect_no_lint("fcase(l1, v1, l2, v2)", linter)
  expect_no_lint("case_when(l1 ~ v1, l2 ~ v2)", linter)
})

test_that("nested_ifelse_linter blocks simple disallowed usages", {
  expect_lint(
    "ifelse(l1, v1, ifelse(l2, v2, v3))",
    rex::rex("Don't use nested ifelse() calls"),
    nested_ifelse_linter()
  )

  expect_lint(
    "ifelse(l1, ifelse(l2, v1, v2), v3)",
    rex::rex("Don't use nested ifelse() calls"),
    nested_ifelse_linter()
  )
})

test_that("nested_ifelse_linter also catches dplyr::if_else", {
  expect_lint(
    "if_else(l1, v1, if_else(l2, v2, v3))",
    rex::rex("Don't use nested if_else() calls"),
    nested_ifelse_linter()
  )

  expect_lint(
    "dplyr::if_else(l1, dplyr::if_else(l2, v1, v2), v3)",
    rex::rex("Don't use nested if_else() calls"),
    nested_ifelse_linter()
  )
})

test_that("nested_ifelse_linter also catches data.table::fifelse", {
  expect_lint(
    "fifelse(l1, v1, fifelse(l2, v2, v3))",
    rex::rex("Don't use nested fifelse() calls"),
    nested_ifelse_linter()
  )

  expect_lint(
    "data.table::fifelse(l1, v1, data.table::fifelse(l2, v2, v3))",
    rex::rex("Don't use nested fifelse() calls"),
    nested_ifelse_linter()
  )

  # not sure why anyone would do this, but the readability still argument holds
  expect_lint(
    "data.table::fifelse(l1, dplyr::if_else(l2, v1, v2), v3)",
    rex::rex("Don't use nested if_else() calls"),
    nested_ifelse_linter()
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      ifelse(x < 0, ifelse(x == 0, 0, 1), -1)
      fifelse(y < 0, fifelse(y == 0, 0, 1), -1)
    }"),
    list(
      list("ifelse", line_number = 2L),
      list("fifelse", line_number = 3L)
    ),
    nested_ifelse_linter()
  )
})
