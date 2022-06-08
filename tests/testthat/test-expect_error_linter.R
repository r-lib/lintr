test_that("expect_error_linter skips allowed usages", {
  expect_lint("expect_error(stop('yes'))", NULL, expect_error_linter())
  expect_lint("expect_error(stop('yes'), 'yes')", NULL, expect_error_linter())
  expect_lint("expect_that(5 * 2, equals(10))", NULL, expect_error_linter())
})

test_that("expect_error_linter produces expected lint", {
  expect_lint(
    "expect_that(x, throws_error())",
    rex::rex("Use expect_error(x) instead of expect_that(x, throws_error())."),
    expect_error_linter()
  )
})
