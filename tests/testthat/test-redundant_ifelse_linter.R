test_that("redundant_ifelse_linter skips allowed usages", {
  expect_lint("ifelse(x > 5, 0, 2)", NULL, redundant_ifelse_linter())
  expect_lint("ifelse(x > 5, TRUE, NA)", NULL, redundant_ifelse_linter())
  expect_lint("ifelse(x > 5, FALSE, NA)", NULL, redundant_ifelse_linter())
  expect_lint("ifelse(x > 5, TRUE, TRUE)", NULL, redundant_ifelse_linter())

  expect_lint("ifelse(x > 5, 0L, 2L)", NULL, redundant_ifelse_linter())
  expect_lint("ifelse(x > 5, 0L, 10L)", NULL, redundant_ifelse_linter())
})

test_that("redundant_ifelse_linter blocks simple disallowed usages", {
  expect_lint(
    "ifelse(x > 5, TRUE, FALSE)",
    rex::rex("Just use the logical condition (or its negation) directly"),
    redundant_ifelse_linter()
  )
  expect_lint(
    "ifelse(x > 5, FALSE, TRUE)",
    rex::rex("Just use the logical condition (or its negation) directly"),
    redundant_ifelse_linter()
  )

  # other ifelse equivalents from common packages
  expect_lint(
    "if_else(x > 5, TRUE, FALSE)",
    rex::rex("Just use the logical condition (or its negation) directly"),
    redundant_ifelse_linter()
  )
  expect_lint(
    "fifelse(x > 5, FALSE, TRUE)",
    rex::rex("Just use the logical condition (or its negation) directly"),
    redundant_ifelse_linter()
  )
})

test_that("redundant_ifelse_linter blocks usages equivalent to as.numeric, optionally", {
  expect_lint(
    "ifelse(x > 5, 1L, 0L)",
    rex::rex("Prefer as.integer(x) to ifelse(x, 1L, 0L)"),
    redundant_ifelse_linter()
  )
  expect_lint(
    "ifelse(x > 5, 0L, 1L)",
    rex::rex("Prefer as.integer(x) to ifelse(x, 0L, 1L)"),
    redundant_ifelse_linter()
  )

  expect_lint(
    "ifelse(x > 5, 1, 0)",
    rex::rex("Prefer as.numeric(!x) to ifelse(x, 1, 0)"),
    redundant_ifelse_linter()
  )
  expect_lint(
    "ifelse(x > 5, 0, 1)",
    rex::rex("Prefer as.numeric(!x) to ifelse(x, 0, 1)"),
    redundant_ifelse_linter()
  )

  # data.table/dplyr equivalents
  expect_lint(
    "dplyr::if_else(x > 5, 1L, 0L)",
    rex::rex("Prefer as.integer(x) to if_else(x, 1L, 0L)"),
    redundant_ifelse_linter()
  )
  expect_lint(
    "data.table::fifelse(x > 5, 0L, 1L)",
    rex::rex("Prefer as.integer(x) to fifelse(x, 0L, 1L)"),
    redundant_ifelse_linter()
  )

  expect_lint(
    "if_else(x > 5, 1, 0)",
    rex::rex("Prefer as.numeric(!x) to if_else(x, 1, 0)"),
    redundant_ifelse_linter()
  )
  expect_lint(
    "fifelse(x > 5, 0, 1)",
    rex::rex("Prefer as.numeric(!x) to fifelse(x, 0, 1)"),
    redundant_ifelse_linter()
  )

  expect_lint("ifelse(x > 5, 1L, 0L)", NULL, redundant_ifelse_linter(allow10 = TRUE))
  expect_lint("ifelse(x > 5, 0L, 1L)", NULL, redundant_ifelse_linter(allow10 = TRUE))

  expect_lint("ifelse(x > 5, 1, 0)", NULL, redundant_ifelse_linter(allow10 = TRUE))
  expect_lint("ifelse(x > 5, 0, 1)", NULL, redundant_ifelse_linter(allow10 = TRUE))

  expect_lint("dplyr::if_else(x > 5, 1L, 0L)", NULL, redundant_ifelse_linter(allow10 = TRUE))
  expect_lint("data.table::fifelse(x > 5, 0L, 1L)", NULL, redundant_ifelse_linter(allow10 = TRUE))

  expect_lint("if_else(x > 5, 1, 0)", NULL, redundant_ifelse_linter(allow10 = TRUE))
  expect_lint("fifelse(x > 5, 0, 1)", NULL, redundant_ifelse_linter(allow10 = TRUE))
})
