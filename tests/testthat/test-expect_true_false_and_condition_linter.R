test_that("expect_true_false_and_condition_linter skips allowed usages", {
  expect_lint("expect_true(x)", NULL, expect_true_false_and_condition_linter())
  expect_lint("testthat::expect_false(x, y, z)", NULL, expect_true_false_and_condition_linter())

  # more complicated expression
  expect_lint("expect_true(x || (y && z))", NULL, expect_true_false_and_condition_linter())
  # the same by operator precedence, though not obvious a priori
  expect_lint("expect_false(x || y && z)", NULL, expect_true_false_and_condition_linter())
  expect_lint("expect_true(x && y || z)", NULL, expect_true_false_and_condition_linter())
})

test_that("expect_true_false_and_condition_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_false(x && y)",
    rex::rex("Write multiple && conditions in expect_true()/expect_false() as"),
    expect_true_false_and_condition_linter()
  )

  expect_lint(
    "expect_true(x && y && z)",
    rex::rex("Write multiple && conditions in expect_true()/expect_false() as"),
    expect_true_false_and_condition_linter()
  )
})
