test_that("consecutive_stopifnot_linter skips allowed usages", {
  linter <- consecutive_stopifnot_linter()
  msg <- rex::rex("Unify consecutive calls to stopifnot().")

  expect_lint("stopifnot(x)", NULL, linter)
  expect_lint("stopifnot(x, y, z)", NULL, linter)

  # intervening expression
  expect_lint("stopifnot(x); y; stopifnot(z)", NULL, linter)

  # inline or potentially with gaps don't matter
  lines <- trim_some("
    stopifnot(x)
    y

    stopifnot(z)
  ")
  expect_lint(lines, NULL, linter)
})

test_that("consecutive_stopifnot_linter blocks simple disallowed usages", {
  linter <- consecutive_stopifnot_linter()
  msg <- rex::rex("Unify consecutive calls to stopifnot().")

  # one test of inline usage
  expect_lint("stopifnot(x); stopifnot(y)", msg, linter)

  lines_gap <- trim_some("
    stopifnot(x)

    stopifnot(y, z)
  ")
  expect_lint(lines_gap, msg, linter)

  lines_consecutive <- trim_some("
    stopifnot(x)
    stopifnot(y)
  ")
  expect_lint(lines_consecutive, msg, linter)

  lines_comment <- trim_some("
    stopifnot(x)
    # a comment on y
    stopifnot(y)
  ")
  expect_lint(lines_comment, msg, linter)
})
