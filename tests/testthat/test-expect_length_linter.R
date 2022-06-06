test_that("expect_length_linter skips allowed usages", {
  expect_lint("expect_equal(nrow(x), 4L)", NULL, expect_length_linter())
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint("testthat::expect_equal(nrow(x), 4L)", NULL, expect_length_linter())

  # only check the first argument. yoda tests in the second argument will be
  #   missed, but there are legitimate uses of length() in argument 2
  expect_lint("expect_equal(nrow(x), length(y))", NULL, expect_length_linter())

  # expect_length() doesn't have info= or label= arguments
  expect_lint("expect_equal(length(x), n, info = 'x should have size n')", NULL, expect_length_linter())
  expect_lint("expect_equal(length(x), n, label = 'x size')", NULL, expect_length_linter())
  expect_lint("expect_equal(length(x), n, expected.label = 'target size')", NULL, expect_length_linter())
})

test_that("expect_length_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_equal(length(x), 2L)",
    rex::rex("expect_length(x, n) is better than expect_equal(length(x), n)"),
    expect_length_linter()
  )

  expect_lint(
    "testthat::expect_equal(length(DF), length(old))",
    rex::rex("expect_length(x, n) is better than expect_equal(length(x), n)"),
    expect_length_linter()
  )

  # yoda test cases
  expect_lint(
    "expect_equal(2, length(x))",
    rex::rex("expect_length(x, n) is better than expect_equal(length(x), n)"),
    expect_length_linter()
  )

  expect_lint(
    "expect_equal(2L, length(x))",
    rex::rex("expect_length(x, n) is better than expect_equal(length(x), n)"),
    expect_length_linter()
  )
})

test_that("expect_length_linter blocks expect_identical usage as well", {
  expect_lint(
    "expect_identical(length(x), 2L)",
    rex::rex("expect_length(x, n) is better than expect_identical(length(x), n)"),
    expect_length_linter()
  )
})
