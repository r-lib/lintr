test_that("expect_true_false_linter skips allowed usages", {
  # expect_true is a scalar test; testing logical vectors with expect_equal is OK
  expect_lint("expect_equal(x, c(TRUE, FALSE))", NULL, expect_true_false_linter())
})

test_that("expect_true_false_linter blocks simple disallowed usages", {
  linter <- expect_true_false_linter()

  expect_lint(
    "expect_equal(foo(x), TRUE)",
    rex::rex("expect_true(x) is better than expect_equal(x, TRUE)"),
    linter
  )

  # expect_identical is treated the same as expect_equal
  expect_lint(
    "testthat::expect_identical(x, FALSE)",
    rex::rex("expect_false(x) is better than expect_identical(x, FALSE)"),
    linter
  )

  # also caught when TRUE/FALSE is the first argument
  expect_lint(
    "expect_equal(TRUE, foo(x))",
    rex::rex("expect_true(x) is better than expect_equal(x, TRUE)"),
    linter
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      expect_equal(x, TRUE)
      expect_equal(x, FALSE)
    }"),
    list(
      list("expect_true", line_number = 2L),
      list("expect_false", line_number = 3L)
    ),
    expect_true_false_linter()
  )
})
