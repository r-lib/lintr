test_that("expect_not_linter skips allowed usages", {
  expect_lint("expect_true(x)", NULL, expect_not_linter())
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint("testthat::expect_true(x)", NULL, expect_not_linter())
  expect_lint("expect_false(x)", NULL, expect_not_linter())
  expect_lint("testthat::expect_false(x)", NULL, expect_not_linter())

  # not a strict ban on !
  ##  (expect_false(x && y) is the same, but it's not clear which to prefer)
  expect_lint("expect_true(!x || !y)", NULL, expect_not_linter())
})

test_that("expect_not_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_true(!x)",
    "expect_false\\(x\\) is better than expect_true\\(!x\\)",
    expect_not_linter()
  )

  expect_lint(
    "testthat::expect_true(!x)",
    "expect_false\\(x\\) is better than expect_true\\(!x\\)",
    expect_not_linter()
  )

  expect_lint(
    "expect_false(!foo(x))",
    "expect_false\\(x\\) is better than expect_true\\(!x\\)",
    expect_not_linter()
  )

  expect_lint(
    "testthat::expect_true(!(x && y))",
    "expect_false\\(x\\) is better than expect_true\\(!x\\)",
    expect_not_linter()
  )
})
