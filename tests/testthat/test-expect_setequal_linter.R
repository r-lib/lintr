test_that("expect_setequal_linter skips allowed usages", {
  # tolerance= blocks applicability
  expect_lint("expect_equal(sort(x), sort(y), tolerance = 1e-12)", NULL, expect_setequal_linter())
})

test_that("expect_setequal_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_identical(sort(x), sort(y))",
    rex::rex("Use expect_setequal(actual, expected) instead of expect_identical(sort(actual), expected)"),
    expect_setequal_linter()
  )
  expect_lint(
    "expect_equal(sort(x), sort(y))",
    rex::rex("Use expect_setequal(actual, expected) instead of expect_equal(sort(actual), expected)"),
    expect_setequal_linter()
  )

  # usage in either argument alone is matched
  expect_lint(
    "expect_identical(x, sort(y))",
    rex::rex("Use expect_setequal(actual, expected) instead of expect_identical(sort(actual), expected)"),
    expect_setequal_linter()
  )
  expect_lint(
    "expect_identical(sort(x), y)",
    rex::rex("Use expect_setequal(actual, expected) instead of expect_identical(sort(actual), expected)"),
    expect_setequal_linter()
  )
})
