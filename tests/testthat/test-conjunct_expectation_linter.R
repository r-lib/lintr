test_that("conjunct_expectation_linter skips allowed usages of expect_true", {
  expect_lint("expect_true(x)", NULL, conjunct_expectation_linter())
  expect_lint("testthat::expect_true(x, y, z)", NULL, conjunct_expectation_linter())

  # more complicated expression
  expect_lint("expect_true(x || (y && z))", NULL, conjunct_expectation_linter())
  # the same by operator precedence, though not obvious a priori
  expect_lint("expect_true(x || y && z)", NULL, conjunct_expectation_linter())
  expect_lint("expect_true(x && y || z)", NULL, conjunct_expectation_linter())
})

test_that("conjunct_expectation_linter skips allowed usages of expect_true", {
  expect_lint("expect_false(x)", NULL, conjunct_expectation_linter())
  expect_lint("testthat::expect_false(x, y, z)", NULL, conjunct_expectation_linter())

  # more complicated expression
  # (NB: xx && yy || zz and xx || yy && zz both parse with || first)
  expect_lint("expect_false(x && (y || z))", NULL, conjunct_expectation_linter())
})

test_that("conjunct_expectation_linter blocks && conditions with expect_true()", {
  expect_lint(
    "expect_true(x && y)",
    rex::rex("Instead of expect_true(A && B), write multiple expectations"),
    conjunct_expectation_linter()
  )

  expect_lint(
    "expect_true(x && y && z)",
    rex::rex("Instead of expect_true(A && B), write multiple expectations"),
    conjunct_expectation_linter()
  )
})

test_that("conjunct_expectation_linter blocks || conditions with expect_false()", {
  expect_lint(
    "expect_false(x || y)",
    rex::rex("Instead of expect_false(A || B), write multiple expectations"),
    conjunct_expectation_linter()
  )

  expect_lint(
    "expect_false(x || y || z)",
    rex::rex("Instead of expect_false(A || B), write multiple expectations"),
    conjunct_expectation_linter()
  )

  # these lint because `||` is always outer by operator precedence
  expect_lint(
    "expect_false(x || y && z)",
    rex::rex("Instead of expect_false(A || B), write multiple expectations"),
    conjunct_expectation_linter()
  )
  expect_lint(
    "expect_false(x && y || z)",
    rex::rex("Instead of expect_false(A || B), write multiple expectations"),
    conjunct_expectation_linter()
  )
})
