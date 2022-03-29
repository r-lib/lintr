test_that("conjunct_test_linter skips allowed usages of expect_true", {
  expect_lint("expect_true(x)", NULL, conjunct_test_linter())
  expect_lint("testthat::expect_true(x, y, z)", NULL, conjunct_test_linter())

  # more complicated expression
  expect_lint("expect_true(x || (y && z))", NULL, conjunct_test_linter())
  # the same by operator precedence, though not obvious a priori
  expect_lint("expect_true(x || y && z)", NULL, conjunct_test_linter())
  expect_lint("expect_true(x && y || z)", NULL, conjunct_test_linter())
})

test_that("conjunct_test_linter skips allowed usages of expect_true", {
  expect_lint("expect_false(x)", NULL, conjunct_test_linter())
  expect_lint("testthat::expect_false(x, y, z)", NULL, conjunct_test_linter())

  # more complicated expression
  # (NB: xx && yy || zz and xx || yy && zz both parse with || first)
  expect_lint("expect_false(x && (y || z))", NULL, conjunct_test_linter())
})

test_that("conjunct_test_linter blocks && conditions with expect_true()", {
  expect_lint(
    "expect_true(x && y)",
    rex::rex("Instead of expect_true(A && B), write multiple expectations"),
    conjunct_test_linter()
  )

  expect_lint(
    "expect_true(x && y && z)",
    rex::rex("Instead of expect_true(A && B), write multiple expectations"),
    conjunct_test_linter()
  )
})

test_that("conjunct_test_linter blocks || conditions with expect_false()", {
  expect_lint(
    "expect_false(x || y)",
    rex::rex("Instead of expect_false(A || B), write multiple expectations"),
    conjunct_test_linter()
  )

  expect_lint(
    "expect_false(x || y || z)",
    rex::rex("Instead of expect_false(A || B), write multiple expectations"),
    conjunct_test_linter()
  )

  # these lint because `||` is always outer by operator precedence
  expect_lint(
    "expect_false(x || y && z)",
    rex::rex("Instead of expect_false(A || B), write multiple expectations"),
    conjunct_test_linter()
  )
  expect_lint(
    "expect_false(x && y || z)",
    rex::rex("Instead of expect_false(A || B), write multiple expectations"),
    conjunct_test_linter()
  )
})

test_that("conjunct_test_linter skips allowed stopifnot() and assert_that() usages", {
  expect_lint("stopifnot(x)", NULL, conjunct_test_linter())
  expect_lint("assert_that(x, y, z)", NULL, conjunct_test_linter())

  # more complicated expression
  expect_lint("stopifnot(x || (y && z))", NULL, conjunct_test_linter())
  # the same by operator precedence, though not obvious a priori
  expect_lint("stopifnot(x || y && z)", NULL, conjunct_test_linter())
  expect_lint("assertthat::assert_that(x && y || z)", NULL, conjunct_test_linter())
})

test_that("conjunct_test_linter blocks simple disallowed usages of stopifnot() and assert_that()", {
  expect_lint(
    "stopifnot(x && y)",
    rex::rex("Instead of stopifnot(A && B), write multiple conditions"),
    conjunct_test_linter()
  )

  expect_lint(
    "stopifnot(x && y && z)",
    rex::rex("Instead of stopifnot(A && B), write multiple conditions"),
    conjunct_test_linter()
  )

  # assert_that() versions
  expect_lint(
    "assert_that(x && y)",
    rex::rex("Instead of assert_that(A && B), write multiple conditions"),
    conjunct_test_linter()
  )

  expect_lint(
    "assertthat::assert_that(x && y && z)",
    rex::rex("Instead of assert_that(A && B), write multiple conditions"),
    conjunct_test_linter()
  )
})
