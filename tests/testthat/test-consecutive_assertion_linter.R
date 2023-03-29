test_that("consecutive_assertion_linter skips allowed usages", {
  expect_lint("stopifnot(x)", NULL, consecutive_assertion_linter())
  expect_lint("stopifnot(x, y, z)", NULL, consecutive_assertion_linter())

  # intervening expression
  expect_lint("stopifnot(x); y; stopifnot(z)", NULL, consecutive_assertion_linter())

  # inline or potentially with gaps don't matter
  lines <- trim_some("
    stopifnot(x)
    y

    stopifnot(z)
  ")
  expect_lint(lines, NULL, consecutive_assertion_linter())
})

test_that("consecutive_assertion_linter blocks simple disallowed usages", {
  # one test of inline usage
  expect_lint(
    "stopifnot(x); stopifnot(y)",
    rex::rex("Unify consecutive calls to stopifnot()."),
    consecutive_assertion_linter()
  )

  lines_gap <- trim_some("
    stopifnot(x)

    stopifnot(y, z)
  ")
  expect_lint(
    lines_gap,
    rex::rex("Unify consecutive calls to stopifnot()."),
    consecutive_assertion_linter()
  )

  lines_consecutive <- trim_some("
    stopifnot(x)
    stopifnot(y)
  ")
  expect_lint(
    lines_consecutive,
    rex::rex("Unify consecutive calls to stopifnot()."),
    consecutive_assertion_linter()
  )

  lines_comment <- trim_some("
    stopifnot(x)
    # a comment on y
    stopifnot(y)
  ")
  expect_lint(
    lines_comment,
    rex::rex("Unify consecutive calls to stopifnot()."),
    consecutive_assertion_linter()
  )
})

test_that("assert_that usages are handled correctly too", {
  linter <- consecutive_assertion_linter()
  lint_msg <- rex::rex("Unify consecutive calls to assert_that().")

  expect_lint("assert_that(x)", NULL, consecutive_assertion_linter())
  expect_lint("assertthat::assert_that(x, y, z)", NULL, consecutive_assertion_linter())

  # if msg= is used, can't necessarily combine
  lines <- trim_some("
    assert_that(x, msg = 'bad x')
    assert_that(y, msg = 'bad y')
  ")
  expect_lint(lines, NULL, consecutive_assertion_linter())
 
  # one test of inline usage
  expect_lint(
    "assert_that(x); assert_that(y)",
    lint_msg,
    consecutive_assertion_linter()
  )

  lines_gap <- trim_some("
    assert_that(x)

    assertthat::assert_that(y, z)
  ")
  expect_lint(lines_gap, lint_msg, consecutive_assertion_linter())
})

test_that("Mixing test functions is fine", {
  expect_lint(
    trim_some("
      assert_that(x)
      stopifnot(y)
    "),
    NULL,
    consecutive_assertion_linter()
  )
})

test_that("old name consecutive_stopifnot_linter() is deprecated", {
  expect_warning(
    {
      old_linter <- consecutive_stopifnot_linter()
    },
    "Use consecutive_assertion_linter instead",
    fixed = TRUE
  )
  expect_lint('stopifnot(x); y; stopifnot(z)', NULL, old_linter)
  expect_lint("stopifnot(x); stopifnot(y)", "Unify consecutive calls", old_linter)
})
