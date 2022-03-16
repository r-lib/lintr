test_that("expect_comparison_linter skips allowed usages", {
  # there's no expect_ne() for this operator
  expect_lint("expect_true(x != y)", NULL, expect_comparison_linter())
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint(
    "testthat::expect_true(x != y)",
    NULL,
    expect_comparison_linter()
  )

  # multiple comparisons are OK
  expect_lint(
    "expect_true(x > y || x > z)",
    NULL,
    expect_comparison_linter()
  )
})

test_that("expect_comparison_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_true(x > y)",
    rex::rex("expect_gt(x, y) is better than expect_true(x > y)."),
    expect_comparison_linter()
  )

  # namespace qualification is irrelevant
  expect_lint(
    "testthat::expect_true(x < y)",
    rex::rex("expect_gt(x, y) is better than expect_true(x > y)."),
    expect_comparison_linter()
  )

  expect_lint(
    "expect_true(foo(x) >= y[[2]])",
    rex::rex("expect_gt(x, y) is better than expect_true(x > y)."),
    expect_comparison_linter()
  )

  expect_lint(
    "expect_true(x <= y)",
    rex::rex("expect_gt(x, y) is better than expect_true(x > y)."),
    expect_comparison_linter()
  )

  expect_lint(
    "expect_true(x == (y == 2))",
    rex::rex("expect_gt(x, y) is better than expect_true(x > y)."),
    expect_comparison_linter()
  )
})
