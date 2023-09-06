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

test_that("conjunct_test_linter's allow_named_stopifnot argument works", {
  # allowed by default
  expect_lint(
    "stopifnot('x must be a logical scalar' = length(x) == 1 && is.logical(x) && !is.na(x))",
    NULL,
    conjunct_test_linter()
  )
  expect_lint(
    "stopifnot('x is a logical scalar' = length(x) == 1 && is.logical(x) && !is.na(x))",
    rex::rex("Instead of stopifnot(A && B), write multiple conditions"),
    conjunct_test_linter(allow_named_stopifnot = FALSE)
  )
})

test_that("conjunct_test_linter skips allowed usages", {
  linter <- conjunct_test_linter()

  expect_lint("dplyr::filter(DF, A, B)", NULL, linter)
  expect_lint("dplyr::filter(DF, !(A & B))", NULL, linter)
  # | is the "top-level" operator here
  expect_lint("dplyr::filter(DF, A & B | C)", NULL, linter)
  expect_lint("dplyr::filter(DF, A | B & C)", NULL, linter)
})

test_that("conjunct_test_linter blocks simple disallowed usages", {
  linter <- conjunct_test_linter()
  lint_msg <- rex::rex("Use dplyr::filter(DF, A, B) instead of dplyr::filter(DF, A & B)")

  expect_lint("dplyr::filter(DF, A & B)", lint_msg, linter)
  expect_lint("dplyr::filter(DF, A & B & C)", lint_msg, linter)

  # more common usage, in pipes
  expect_lint("DF %>% dplyr::filter(A & B)", lint_msg, linter)
})

test_that("conjunct_test_linter respects its allow_filter argument", {
  linter <- conjunct_test_linter(allow_filter = TRUE)

  expect_lint("dplyr::filter(DF, A & B)", NULL, linter)
  expect_lint("dplyr::filter(DF, A & B & C)", NULL, linter)
  expect_lint("DF %>% dplyr::filter(A & B)", NULL, linter)
})

test_that("filter() is assumed to be dplyr::filter() by default, unless o/w specified", {
  linter <- conjunct_test_linter()

  expect_lint("stats::filter(A & B)", NULL, linter)
  expect_lint("ns::filter(A & B)", NULL, linter)
  expect_lint(
    "DF %>% filter(A & B)",
    rex::rex("Use dplyr::filter(DF, A, B) instead of dplyr::filter(DF, A & B)"),
    linter
  )
})
