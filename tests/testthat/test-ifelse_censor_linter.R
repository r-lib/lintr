test_that("ifelse_censor_linter skips allowed usages", {
  expect_lint("ifelse(x > 2, x, y)", NULL, ifelse_censor_linter())
  expect_lint("ifelse(x > 2, x, y)", NULL, ifelse_censor_linter())
})

test_that("ifelse_censor_linter blocks simple disallowed usages", {
  expect_lint(
    "ifelse(x < 0, 0, x)",
    rex::rex("pmax(x, y) is preferable to ifelse(x < y, y, x)"),
    ifelse_censor_linter()
  )
  # other equivalents to base::ifelse()
  expect_lint(
    "if_else(x < 0, 0, x)",
    rex::rex("pmax(x, y) is preferable to if_else(x < y, y, x)"),
    ifelse_censor_linter()
  )
  expect_lint(
    "fifelse(x < 0, 0, x)",
    rex::rex("pmax(x, y) is preferable to fifelse(x < y, y, x)"),
    ifelse_censor_linter()
  )

  # other equivalents for censoring
  expect_lint(
    "ifelse(x <= 0, 0, x)",
    rex::rex("pmax(x, y) is preferable to ifelse(x <= y, y, x)"),
    ifelse_censor_linter()
  )
  expect_lint(
    "ifelse(x > 0, x, 0)",
    rex::rex("pmax(x, y) is preferable to ifelse(x > y, x, y)"),
    ifelse_censor_linter()
  )
  expect_lint(
    "ifelse(x >= 0, x, 0)",
    rex::rex("pmax(x, y) is preferable to ifelse(x >= y, x, y)"),
    ifelse_censor_linter()
  )

  # pairwise min/max (similar to censoring)
  expect_lint(
    "ifelse(x < y, x, y)",
    rex::rex("pmin(x, y) is preferable to ifelse(x < y, x, y)"),
    ifelse_censor_linter()
  )
  expect_lint(
    "ifelse(x >= y, y, x)",
    rex::rex("pmin(x, y) is preferable to ifelse(x >= y, y, x)"),
    ifelse_censor_linter()
  )

  # more complicated expression still matches
  lines <- trim_some("
    ifelse(2 + p + 104 + 1 > ncols,
           ncols, 2 + p + 104 + 1
           )
  ")
  expect_lint(
    lines,
    rex::rex("pmin(x, y) is preferable to ifelse(x > y, y, x)"),
    ifelse_censor_linter()
  )
})

# TODO(michaelchirico): how easy would it be to strip parens when considering lint?
#   e.g. ifelse(x < (kMaxIndex - 1), x, kMaxIndex - 1)
