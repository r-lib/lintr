linter <- conjunct_test_linter()

test_that("conjunct_test_linter skips allowed usages of expect_true", {
  expect_lint("expect_true(x)", NULL, linter)
  expect_lint("testthat::expect_true(x, y, z)", NULL, linter)

  # more complicated expression
  expect_lint("expect_true(x || (y && z))", NULL, linter)
  # the same by operator precedence, though not obvious a priori
  expect_lint("expect_true(x || y && z)", NULL, linter)
  expect_lint("expect_true(x && y || z)", NULL, linter)
})

test_that("conjunct_test_linter skips allowed usages of expect_true", {
  expect_lint("expect_false(x)", NULL, linter)
  expect_lint("testthat::expect_false(x, y, z)", NULL, linter)

  # more complicated expression
  # (NB: xx && yy || zz and xx || yy && zz both parse with || first)
  expect_lint("expect_false(x && (y || z))", NULL, linter)
})

test_that("conjunct_test_linter blocks && conditions with expect_true()", {
  msg <- rex::rex("Instead of expect_true(A && B), write multiple expectations")

  expect_lint("expect_true(x && y)", msg, linter)
  expect_lint("expect_true(x && y && z)", msg, linter)
})

test_that("conjunct_test_linter blocks || conditions with expect_false()", {
  msg <- rex::rex("Instead of expect_false(A || B), write multiple expectations")

  expect_lint("expect_false(x || y)", msg, linter)
  expect_lint("expect_false(x || y || z)", msg, linter)

  # these lint because `||` is always outer by operator precedence
  expect_lint("expect_false(x || y && z)", msg, linter)
  expect_lint("expect_false(x && y || z)", msg, linter)
})

test_that("conjunct_test_linter skips allowed stopifnot() and assert_that() usages", {
  expect_lint("stopifnot(x)", NULL, linter)
  expect_lint("assert_that(x, y, z)", NULL, linter)

  # more complicated expression
  expect_lint("stopifnot(x || (y && z))", NULL, linter)
  # the same by operator precedence, though not obvious a priori
  expect_lint("stopifnot(x || y && z)", NULL, linter)
  expect_lint("assertthat::assert_that(x && y || z)", NULL, linter)
})

test_that("conjunct_test_linter blocks simple disallowed usages of stopifnot() and assert_that()", {
  msg_stopifnot <- rex::rex("Instead of stopifnot(A && B), write multiple conditions")
  msg_assertthat <- rex::rex("Instead of assert_that(A && B), write multiple conditions")

  expect_lint("stopifnot(x && y)", msg_stopifnot, linter)
  expect_lint("stopifnot(x && y && z)", msg_stopifnot, linter)

  # assert_that() versions
  expect_lint("assert_that(x && y)", msg_assertthat, linter)
  expect_lint("assertthat::assert_that(x && y && z)", msg_assertthat, linter)
})

test_that("conjunct_test_linter's allow_named_stopifnot argument works", {
  # allowed by default
  expect_lint(
    "stopifnot('x must be a logical scalar' = length(x) == 1 && is.logical(x) && !is.na(x))",
    NULL,
    linter
  )
  expect_lint(
    "stopifnot('x is a logical scalar' = length(x) == 1 && is.logical(x) && !is.na(x))",
    rex::rex("Instead of stopifnot(A && B), write multiple conditions"),
    conjunct_test_linter(allow_named_stopifnot = FALSE)
  )
})
