test_that("expect_s4_class_linter skips allowed usages", {
  linter <- expect_s4_class_linter()

  # expect_s4_class doesn't have an inverted version
  expect_lint("expect_true(!is(x, 'class'))", NULL, linter)
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint("testthat::expect_s3_class(!is(x, 'class'))", NULL, linter)

  # expect_s4_class() doesn't have info= or label= arguments
  expect_lint("expect_true(is(x, 'SpatialPoly'), info = 'x should be SpatialPoly')", NULL, linter)
  expect_lint("expect_true(is(x, 'SpatialPoly'), label = 'x inheritance')", NULL, linter)
})

test_that("expect_s4_class blocks simple disallowed usages", {
  linter <- expect_s4_class_linter()
  lint_msg <- rex::rex("expect_s4_class(x, k) is better than expect_true(is(x, k))")

  expect_lint("expect_true(is(x, 'data.frame'))", lint_msg, linter)
  # namespace qualification is irrelevant
  expect_lint("testthat::expect_true(methods::is(x, 'SpatialPolygonsDataFrame'))", lint_msg, linter)
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("expect_s4_class(x, k) is better than expect_true(is(x, k))")

  expect_lint(
    trim_some("{
      expect_true(is(x, 'data.frame'))
      expect_true(is(x, 'SpatialPolygonsDataFrame'))
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    expect_s4_class_linter()
  )
})
