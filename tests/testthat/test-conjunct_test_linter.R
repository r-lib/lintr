test_that("conjunct_test_linter skips allowed usages of expect_true", {
  linter <- conjunct_test_linter()

  expect_no_lint("expect_true(x)", linter)
  expect_no_lint("testthat::expect_true(x, y, z)", linter)

  # more complicated expression
  expect_no_lint("expect_true(x || (y && z))", linter)
  # the same by operator precedence, though not obvious a priori
  expect_no_lint("expect_true(x || y && z)", linter)
  expect_no_lint("expect_true(x && y || z)", linter)
})

test_that("conjunct_test_linter skips allowed usages of expect_true", {
  linter <- conjunct_test_linter()

  expect_no_lint("expect_false(x)", linter)
  expect_no_lint("testthat::expect_false(x, y, z)", linter)

  # more complicated expression
  # (NB: xx && yy || zz and xx || yy && zz both parse with || first)
  expect_no_lint("expect_false(x && (y || z))", linter)
})

test_that("conjunct_test_linter blocks && conditions with expect_true()", {
  linter <- conjunct_test_linter()
  lint_msg <-
    rex::rex("Write multiple expectations like expect_true(A) and expect_true(B) instead of expect_true(A && B)")

  expect_lint("expect_true(x && y)", lint_msg, linter)
  expect_lint("expect_true(x && y && z)", lint_msg, linter)
})

test_that("conjunct_test_linter blocks || conditions with expect_false()", {
  linter <- conjunct_test_linter()
  lint_msg <-
    rex::rex("Write multiple expectations like expect_false(A) and expect_false(B) instead of expect_false(A || B)")

  expect_lint("expect_false(x || y)", lint_msg, linter)
  expect_lint("expect_false(x || y || z)", lint_msg, linter)

  # these lint because `||` is always outer by operator precedence
  expect_lint("expect_false(x || y && z)", lint_msg, linter)
  expect_lint("expect_false(x && y || z)", lint_msg, linter)
})

test_that("conjunct_test_linter skips allowed stopifnot() and assert_that() usages", {
  linter <- conjunct_test_linter()

  expect_no_lint("stopifnot(x)", linter)
  expect_no_lint("assert_that(x, y, z)", linter)

  # more complicated expression
  expect_no_lint("stopifnot(x || (y && z))", linter)
  # the same by operator precedence, though not obvious a priori
  expect_no_lint("stopifnot(x || y && z)", linter)
  expect_no_lint("assertthat::assert_that(x && y || z)", linter)
})

test_that("conjunct_test_linter blocks simple disallowed usages of stopifnot() and assert_that()", {
  linter <- conjunct_test_linter()
  lint_msg <- function(fun) rex::rex("Write multiple conditions like ", fun, "(A, B) instead of ", fun, "(A && B)")

  expect_lint("stopifnot(x && y)", lint_msg("stopifnot"), linter)
  expect_lint("stopifnot(x && y && z)", lint_msg("stopifnot"), linter)

  # assert_that() versions
  expect_lint("assert_that(x && y)", lint_msg("assert_that"), linter)
  expect_lint("assertthat::assert_that(x && y && z)", lint_msg("assert_that"), linter)
})

test_that("conjunct_test_linter's allow_named_stopifnot argument works", {
  linter <- conjunct_test_linter()

  # allowed by default
  expect_no_lint(
    "stopifnot('x must be a logical scalar' = length(x) == 1 && is.logical(x) && !is.na(x))",
    linter
  )
  # including with intervening comment
  expect_no_lint(
    trim_some("
      stopifnot('x must be a logical scalar' = # comment
        length(x) == 1 && is.logical(x) && !is.na(x)
      )
    "),
    linter
  )

  expect_lint(
    "stopifnot('x is a logical scalar' = length(x) == 1 && is.logical(x) && !is.na(x))",
    rex::rex("Write multiple conditions like stopifnot(A, B)"),
    conjunct_test_linter(allow_named_stopifnot = FALSE)
  )
})

test_that("conjunct_test_linter skips allowed usages", {
  linter <- conjunct_test_linter()

  expect_no_lint("dplyr::filter(DF, A, B)", linter)
  expect_no_lint("dplyr::filter(DF, !(A & B))", linter)
  # | is the "top-level" operator here
  expect_no_lint("dplyr::filter(DF, A & B | C)", linter)
  expect_no_lint("dplyr::filter(DF, A | B & C)", linter)
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
  linter_always <- conjunct_test_linter(allow_filter = "always")
  linter_dplyr <- conjunct_test_linter(allow_filter = "not_dplyr")
  lint_msg <- rex::rex("Use dplyr::filter(DF, A, B) instead of dplyr::filter(DF, A & B)")

  expect_no_lint("dplyr::filter(DF, A & B)", linter_always)
  expect_no_lint("dplyr::filter(DF, A & B & C)", linter_always)
  expect_no_lint("DF %>% dplyr::filter(A & B)", linter_always)
  expect_lint("dplyr::filter(DF, A & B)", lint_msg, linter_dplyr)
  expect_lint("dplyr::filter(DF, A & B & C)", lint_msg, linter_dplyr)
  expect_lint("DF %>% dplyr::filter(A & B)", lint_msg, linter_dplyr)
  expect_no_lint("filter(DF, A & B)", linter_dplyr)
  expect_no_lint("filter(DF, A & B & C)", linter_dplyr)
  expect_no_lint("DF %>% filter(A & B)", linter_dplyr)
})

test_that("filter() is assumed to be dplyr::filter() by default, unless o/w specified", {
  linter <- conjunct_test_linter()

  expect_no_lint("stats::filter(A & B)", linter)
  expect_no_lint("ns::filter(A & B)", linter)
  expect_lint(
    "DF %>% filter(A & B)",
    rex::rex("Use dplyr::filter(DF, A, B) instead of dplyr::filter(DF, A & B)"),
    linter
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      stopifnot(A && B)
      filter(DF, A & B)
    }"),
    list(
      list("stopifnot", line_number = 2L),
      list("filter", line_number = 3L)
    ),
    conjunct_test_linter()
  )
})
