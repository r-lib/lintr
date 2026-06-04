test_that("expect_length_linter skips allowed usages", {
  linter <- expect_length_linter()

  expect_no_lint("expect_equal(nrow(x), 4L)", linter)
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_no_lint("testthat::expect_equal(nrow(x), 4L)", linter)

  # only check the first argument. yoda tests in the second argument will be
  #   missed, but there are legitimate uses of length() in argument 2
  expect_no_lint("expect_equal(nrow(x), length(y))", linter)

  # expect_length() doesn't have info= or label= arguments
  expect_no_lint("expect_equal(length(x), n, info = 'x should have size n')", linter)
  expect_no_lint("expect_equal(length(x), n, label = 'x size')", linter)
  expect_no_lint("expect_equal(length(x), n, expected.label = 'target size')", linter)
})

test_that("expect_length_linter blocks simple disallowed usages", {
  linter <- expect_length_linter()
  lint_msg <- rex::rex("expect_length(x, n) is better than expect_equal(length(x), n)")

  expect_lint("expect_equal(length(x), 2L)", lint_msg, linter)
  expect_lint("testthat::expect_equal(length(DF), length(old))", lint_msg, linter)

  # yoda test cases
  expect_lint("expect_equal(2, length(x))", lint_msg, linter)
  expect_lint("expect_equal(2L, length(x))", lint_msg, linter)
})

test_that("expect_length_linter blocks expect_identical usage as well", {
  expect_lint(
    "expect_identical(length(x), 2L)",
    rex::rex("expect_length(x, n) is better than expect_identical(length(x), n)"),
    expect_length_linter()
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      expect_equal(length(x), n)
      expect_identical(length(x), n)
    }"),
    list(
      list("expect_equal", line_number = 2L),
      list("expect_identical", line_number = 3L)
    ),
    expect_length_linter()
  )
})
