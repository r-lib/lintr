test_that("consecutive_stopifnot_linter skips allowed usages", {
  expect_lint("stopifnot(x)", NULL, consecutive_stopifnot_linter())
  expect_lint("stopifnot(x, y, z)", NULL, consecutive_stopifnot_linter())

  # intervening expression
  expect_lint("stopifnot(x); y; stopifnot(z)", NULL, consecutive_stopifnot_linter())

  # inline or potentially with gaps don't matter
  lines <- trim_some("
    stopifnot(x)
    y

    stopifnot(z)
  ")
  expect_lint(lines, NULL, consecutive_stopifnot_linter())
})

test_that("consecutive_stopifnot_linter blocks simple disallowed usages", {
  # one test of inline usage
  expect_lint(
    "stopifnot(x); stopifnot(y)",
    rex::rex("Unify consecutive calls to stopifnot()."),
    consecutive_stopifnot_linter()
  )

  lines_gap <- trim_some("
    stopifnot(x)

    stopifnot(y, z)
  ")
  expect_lint(
    lines_gap,
    rex::rex("Unify consecutive calls to stopifnot()."),
    consecutive_stopifnot_linter()
  )

  lines_consecutive <- trim_some("
    stopifnot(x)
    stopifnot(y)
  ")
  expect_lint(
    lines_consecutive,
    rex::rex("Unify consecutive calls to stopifnot()."),
    consecutive_stopifnot_linter()
  )

  lines_comment <- trim_some("
    stopifnot(x)
    # a comment on y
    stopifnot(y)
  ")
  expect_lint(
    lines_comment,
    rex::rex("Unify consecutive calls to stopifnot()."),
    consecutive_stopifnot_linter()
  )
})
