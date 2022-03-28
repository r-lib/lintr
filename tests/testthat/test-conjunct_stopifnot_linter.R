test_that("conjunct_stopifnot_linter skips allowed usages", {
  expect_lint("stopifnot(x)", NULL, conjunct_stopifnot_linter())
  expect_lint("stopifnot(x, y, z)", NULL, conjunct_stopifnot_linter())

  # more complicated expression
  expect_lint("stopifnot(x || (y && z))", NULL, conjunct_stopifnot_linter())
  # the same by operator precedence, though not obvious a priori
  expect_lint("stopifnot(x || y && z)", NULL, conjunct_stopifnot_linter())
  expect_lint("stopifnot(x && y || z)", NULL, conjunct_stopifnot_linter())
})

test_that("conjunct_stopifnot_linter blocks simple disallowed usages", {
  expect_lint(
    "stopifnot(x && y)",
    rex::rex("Write multiple && conditions in stopifnot() as separate conditions"),
    conjunct_stopifnot_linter()
  )

  expect_lint(
    "stopifnot(x && y && z)",
    rex::rex("Write multiple && conditions in stopifnot() as separate conditions"),
    conjunct_stopifnot_linter()
  )
})
