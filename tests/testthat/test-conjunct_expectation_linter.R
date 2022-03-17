test_that("conjunct_expectation_linter skips allowed usages", {
  expect_lint("expect_true(x)", NULL, conjunct_expectation_linter())
  expect_lint("testthat::expect_false(x, y, z)", NULL, conjunct_expectation_linter())

  # more complicated expression
  expect_lint("expect_true(x || (y && z))", NULL, conjunct_expectation_linter())
  # the same by operator precedence, though not obvious a priori
  expect_lint("expect_false(x || y && z)", NULL, conjunct_expectation_linter())
  expect_lint("expect_true(x && y || z)", NULL, conjunct_expectation_linter())
})

test_that("conjunct_expectation_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_false(x && y)",
    rex::rex("Write multiple && conditions in expect_true()/expect_false() as"),
    conjunct_expectation_linter()
  )

  expect_lint(
    "expect_true(x && y && z)",
    rex::rex("Write multiple && conditions in expect_true()/expect_false() as"),
    conjunct_expectation_linter()
  )
})
