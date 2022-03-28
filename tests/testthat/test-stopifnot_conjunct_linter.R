test_that("stopifnot_conjunct_linter skips allowed usages", {
  expect_lint("stopifnot(x)", NULL, stopifnot_conjunct_linter())
  expect_lint("stopifnot(x, y, z)", NULL, stopifnot_conjunct_linter())

  # more complicated expression
  expect_lint("stopifnot(x || (y && z))", NULL, stopifnot_conjunct_linter())
  # the same by operator precedence, though not obvious a priori
  expect_lint("stopifnot(x || y && z)", NULL, stopifnot_conjunct_linter())
  expect_lint("stopifnot(x && y || z)", NULL, stopifnot_conjunct_linter())
})

test_that("stopifnot_conjunct_linter blocks simple disallowed usages", {
  expect_lint(
    "stopifnot(x && y)",
    rex::rex("Write multiple && conditions in stopifnot() as separate conditions"),
    stopifnot_conjunct_linter()
  )

  expect_lint(
    "stopifnot(x && y && z)",
    rex::rex("Write multiple && conditions in stopifnot() as separate conditions"),
    stopifnot_conjunct_linter()
  )
})
