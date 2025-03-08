test_that("ifelse_censor_linter skips allowed usages", {
  linter <- ifelse_censor_linter()

  expect_lint("ifelse(x == 2, x, y)", NULL, linter)
  expect_lint("ifelse(x > 2, x, y)", NULL, linter)
})

test_that("ifelse_censor_linter blocks simple disallowed usages", {
  linter <- ifelse_censor_linter()

  expect_lint(
    "ifelse(x < 0, 0, x)",
    rex::rex("pmax(x, y) is preferable to ifelse(x < y, y, x)"),
    linter
  )

  # other equivalents to base::ifelse()
  expect_lint(
    "if_else(x < 0, 0, x)",
    rex::rex("pmax(x, y) is preferable to if_else(x < y, y, x)"),
    linter
  )
  expect_lint(
    "fifelse(x < 0, 0, x)",
    rex::rex("pmax(x, y) is preferable to fifelse(x < y, y, x)"),
    linter
  )

  # other equivalents for censoring
  expect_lint(
    "ifelse(x <= 0, 0, x)",
    rex::rex("pmax(x, y) is preferable to ifelse(x <= y, y, x)"),
    linter
  )
  expect_lint(
    "ifelse(x > 0, x, 0)",
    rex::rex("pmax(x, y) is preferable to ifelse(x > y, x, y)"),
    linter
  )
  expect_lint(
    "ifelse(x >= 0, x, 0)",
    rex::rex("pmax(x, y) is preferable to ifelse(x >= y, x, y)"),
    linter
  )

  # pairwise min/max (similar to censoring)
  expect_lint(
    "ifelse(x < y, x, y)",
    rex::rex("pmin(x, y) is preferable to ifelse(x < y, x, y)"),
    linter
  )
  expect_lint(
    "ifelse(x >= y, y, x)",
    rex::rex("pmin(x, y) is preferable to ifelse(x >= y, y, x)"),
    linter
  )

  # more complicated expression still matches
  expect_lint(
    trim_some("
      ifelse(2 + p + 104 + 1 > ncols,
            ncols, 2 + p + 104 + 1
            )
    "),
    rex::rex("pmin(x, y) is preferable to ifelse(x > y, y, x)"),
    linter
  )

  # including with comments
  expect_lint(
    trim_some("
      ifelse(2 + p + 104 + 1 #comment
      > ncols, ncols, 2 + p + 104 + 1
            )
    "),
    rex::rex("pmin(x, y) is preferable to ifelse(x > y, y, x)"),
    linter
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      ifelse(x >= y, y, x)
      ifelse(x >= 0, x, 0)
    }"),
    list(
      list("pmin", line_number = 2L),
      list("pmax", line_number = 3L)
    ),
    ifelse_censor_linter()
  )
})
