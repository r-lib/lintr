test_that("expect_identical_linter skips allowed usages", {
  # expect_type doesn't have an inverted version
  expect_lint("expect_true(identical(x, y) || identical(y, z))", NULL, expect_identical_linter())
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint("testthat::expect_true(identical(x, y) || identical(y, z))", NULL, expect_identical_linter())

  # expect_equal calls with explicit tolerance= are OK
  expect_lint("expect_equal(x, y, tolerance = 1e-6)", NULL, expect_identical_linter())

  # ditto for check.attributes = FALSE
  expect_lint("expect_equal(x, y, check.attributes = FALSE)", NULL, expect_identical_linter())
})

test_that("expect_identical_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_equal(x, y)",
    rex::rex("Use expect_identical(x, y) by default; resort to expect_equal() only when needed"),
    expect_identical_linter()
  )

  # different usage to redirect to expect_identical
  expect_lint(
    "expect_true(identical(x, y))",
    rex::rex("Use expect_identical(x, y) by default; resort to expect_equal() only when needed"),
    expect_identical_linter()
  )
})

test_that("expect_identical_linter skips cases likely testing numeric equality", {
  expect_lint("expect_equal(x, 1.034)", NULL, expect_identical_linter())
  expect_lint("expect_equal(x, c(1.01, 1.02))", NULL, expect_identical_linter())
  # whole numbers with explicit decimals are OK, even in mixed scenarios
  expect_lint("expect_equal(x, c(1.0, 2))", NULL, expect_identical_linter())
  # plain numbers are still caught, however
  expect_lint(
    "expect_equal(x, 1L)",
    rex::rex("Use expect_identical(x, y) by default; resort to expect_equal() only when needed"),
    expect_identical_linter()
  )
  expect_lint(
    "expect_equal(x, 1)",
    rex::rex("Use expect_identical(x, y) by default; resort to expect_equal() only when needed"),
    expect_identical_linter()
  )
  # NB: TRUE is a NUM_CONST so we want test matching it, even though this test is
  #   also a violation of expect_true_false_linter()
  expect_lint(
    "expect_equal(x, TRUE)",
    rex::rex("Use expect_identical(x, y) by default; resort to expect_equal() only when needed"),
    expect_identical_linter()
  )
})

test_that("expect_identical_linter skips 3e cases needing expect_equal", {
  expect_lint("expect_equal(x, y, ignore_attr = 'names')", NULL, expect_identical_linter())
})

# this arose where a helper function was wrapping expect_equal() and
#   some of the "allowed" arguments were being passed --> false positive
test_that("expect_identical_linter skips calls using ...", {
  expect_lint("expect_equal(x, y, ...)", NULL, expect_identical_linter())
})
