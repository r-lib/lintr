test_that("expect_null_linter skips allowed usages", {
  # NB: this usage should be expect_false(), but this linter should
  #   nevertheless apply (e.g. for maintainers not using ExpectNotLinter)
  expect_lint("expect_true(!is.null(x))", NULL, expect_null_linter)
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint(
    "testthat::expect_true(!is.null(x))",
    NULL,
    expect_null_linter
  )

  # length-0 could be NULL, but could be integer() or list(), so let it pass
  expect_lint("expect_length(x, 0L)", NULL, expect_null_linter)
})

test_that("expect_null_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_equal(x, NULL)",
    "expect_null\\(x\\) is better than expect_equal\\(x, NULL\\)",
    expect_null_linter
  )

  # expect_identical is treated the same as expect_equal
  expect_lint(
    "testthat::expect_identical(x, NULL)",
    "expect_null\\(x\\) is better than expect_equal\\(x, NULL\\)",
    expect_null_linter
  )

  # different equivalent usage
  expect_lint(
    "expect_true(is.null(foo(x)))",
    "expect_null\\(x\\) is better than expect_equal\\(x, NULL\\)",
    expect_null_linter
  )
})
