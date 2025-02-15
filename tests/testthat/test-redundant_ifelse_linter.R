test_that("redundant_ifelse_linter skips allowed usages", {
  linter <- redundant_ifelse_linter()

  expect_lint("ifelse(x > 5, 0, 2)", NULL, linter)
  expect_lint("ifelse(x > 5, TRUE, NA)", NULL, linter)
  expect_lint("ifelse(x > 5, FALSE, NA)", NULL, linter)
  expect_lint("ifelse(x > 5, TRUE, TRUE)", NULL, linter)

  expect_lint("ifelse(x > 5, 0L, 2L)", NULL, linter)
  expect_lint("ifelse(x > 5, 0L, 10L)", NULL, linter)
})

test_that("redundant_ifelse_linter blocks simple disallowed usages", {
  linter <- redundant_ifelse_linter()

  expect_lint(
    "ifelse(x > 5, TRUE, FALSE)",
    rex::rex("Just use the logical condition (or its negation) directly"),
    linter
  )
  expect_lint(
    "ifelse(x > 5, FALSE, TRUE)",
    rex::rex("Just use the logical condition (or its negation) directly"),
    linter
  )

  # other ifelse equivalents from common packages
  expect_lint(
    "if_else(x > 5, TRUE, FALSE)",
    rex::rex("Just use the logical condition (or its negation) directly"),
    linter
  )
  expect_lint(
    "fifelse(x > 5, FALSE, TRUE)",
    rex::rex("Just use the logical condition (or its negation) directly"),
    linter
  )
})

test_that("redundant_ifelse_linter blocks usages equivalent to as.numeric, optionally", {
  linter <- redundant_ifelse_linter()

  expect_lint(
    "ifelse(x > 5, 1L, 0L)",
    rex::rex("Prefer as.integer(x) to ifelse(x, 1L, 0L)"),
    linter
  )
  expect_lint(
    "ifelse(x > 5, 0L, 1L)",
    rex::rex("Prefer as.integer(!x) to ifelse(x, 0L, 1L)"),
    linter
  )

  expect_lint(
    "ifelse(x > 5, 1, 0)",
    rex::rex("Prefer as.numeric(x) to ifelse(x, 1, 0)"),
    linter
  )
  expect_lint(
    "ifelse(x > 5, 0, 1)",
    rex::rex("Prefer as.numeric(!x) to ifelse(x, 0, 1)"),
    linter
  )

  # mixing int and num
  expect_lint(
    "ifelse(x > 5, 0, 1L)",
    rex::rex("Prefer as.numeric(!x) to ifelse(x, 0, 1L)"),
    linter
  )
  expect_lint(
    "ifelse(x > 5, 0L, 1)",
    rex::rex("Prefer as.numeric(!x) to ifelse(x, 0L, 1)"),
    linter
  )
  expect_lint(
    "ifelse(x > 5, 1, 0L)",
    rex::rex("Prefer as.numeric(x) to ifelse(x, 1, 0L)"),
    linter
  )
  expect_lint(
    "ifelse(x > 5, 1L, 0)",
    rex::rex("Prefer as.numeric(x) to ifelse(x, 1L, 0)"),
    linter
  )

  # data.table/dplyr equivalents
  expect_lint(
    "dplyr::if_else(x > 5, 1L, 0L)",
    rex::rex("Prefer as.integer(x) to if_else(x, 1L, 0L)"),
    linter
  )
  expect_lint(
    "data.table::fifelse(x > 5, 0L, 1L)",
    rex::rex("Prefer as.integer(!x) to fifelse(x, 0L, 1L)"),
    linter
  )

  expect_lint(
    "if_else(x > 5, 1, 0)",
    rex::rex("Prefer as.numeric(x) to if_else(x, 1, 0)"),
    linter
  )
  expect_lint(
    "fifelse(x > 5, 0, 1)",
    rex::rex("Prefer as.numeric(!x) to fifelse(x, 0, 1)"),
    linter
  )
})

test_that("allow10 works as intended", {
  linter <- redundant_ifelse_linter(allow10 = TRUE)

  expect_lint("ifelse(x > 5, 1L, 0L)", NULL, linter)
  expect_lint("ifelse(x > 5, 0L, 1L)", NULL, linter)

  expect_lint("ifelse(x > 5, 1, 0)", NULL, linter)
  expect_lint("ifelse(x > 5, 0, 1)", NULL, linter)

  expect_lint("dplyr::if_else(x > 5, 1L, 0L)", NULL, linter)
  expect_lint("data.table::fifelse(x > 5, 0L, 1L)", NULL, linter)

  expect_lint("if_else(x > 5, 1, 0)", NULL, linter)
  expect_lint("fifelse(x > 5, 0, 1)", NULL, linter)
})

test_that("ifelse(missing = ) gives correct lints", {
  linter <- redundant_ifelse_linter()

  expect_lint("if_else(x > 5, TRUE, FALSE, NA)", rex::rex("Just use the logical condition"), linter)
  expect_lint("if_else(x > 5, TRUE, FALSE, TRUE)", NULL, linter)
  expect_lint("if_else(x > 5, TRUE, FALSE, 5L)", NULL, linter)

  expect_lint("if_else(x > 5, 1L, 0L, NA_integer_)", rex::rex("Prefer as.integer(x)"), linter)
  expect_lint("if_else(x > 5, 1L, 0L, 2L)", NULL, linter)
  expect_lint("if_else(x > 5, 1L, 0L, 5)", NULL, linter)

  expect_lint("if_else(x > 5, 1, 0, NA_real_)", rex::rex("Prefer as.numeric(x)"), linter)
  expect_lint("if_else(x > 5, 1, 0, 2)", NULL, linter)
  expect_lint("if_else(x > 5, 1, 0, '5')", NULL, linter)

  # TRUE/FALSE must be found in yes/no, not missing=
  expect_lint("if_else(x > 5, 'a', TRUE, FALSE)", NULL, linter)
  expect_lint("if_else(x > 5, 'a', 0L, 1L)", NULL, linter)
  expect_lint("if_else(x > 5, 'a', 1, 0)", NULL, linter)
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      ifelse(x > 0, TRUE, FALSE)
      fifelse(y == 0, 1, 0)
    }"),
    list(
      list("Just use the logical condition", line_number = 2L),
      list(rex::rex("refer as.numeric(x)"), line_number = 3L)
    ),
    redundant_ifelse_linter()
  )
})
