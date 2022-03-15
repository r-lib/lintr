test_that("expect_s3_class_linter skips allowed usages", {
  # expect_s3_class doesn't have an inverted version
  expect_lint("expect_true(!inherits(x, 'class'))", NULL, expect_s3_class_linter())
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint("testthat::expect_true(!inherits(x, 'class'))", NULL, expect_s3_class_linter())

  # other is.<x> calls are not suitable for expect_s3_class in particular
  expect_lint("expect_true(is.na(x))", NULL, expect_s3_class_linter())

  # case where expect_s3_class() *could* be used but we don't enforce
  expect_lint("expect_true(is.data.table(x))", NULL, expect_s3_class_linter())
})

test_that("expect_s3_class_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_equal(class(x), 'data.frame')",
    rex::rex("expect_s3_class(x, k) is better than expect_equal(class(x), k)"),
    expect_s3_class_linter()
  )

  # expect_identical is treated the same as expect_equal
  expect_lint(
    "testthat::expect_identical(class(x), 'lm')",
    rex::rex("expect_s3_class(x, k) is better than expect_identical(class(x), k)"),
    expect_s3_class_linter()
  )

  # yoda test with string literal in first arg also caught
  expect_lint(
    "expect_equal('data.frame', class(x))",
    rex::rex("expect_s3_class(x, k) is better than expect_equal(class(x), k)"),
    expect_s3_class_linter()
  )

  # different equivalent usages
  expect_lint(
    "expect_true(is.table(foo(x)))",
    rex::rex("expect_s3_class(x, k) is better than expect_true(is.<k>(x))"),
    expect_s3_class_linter()
  )
  expect_lint(
    "expect_true(inherits(x, 'table'))",
    rex::rex("expect_s3_class(x, k) is better than expect_true(is.<k>(x))"),
    expect_s3_class_linter()
  )

  # TODO(michaelchirico): consider more carefully which sorts of class(x) %in% . and
  #   . %in% class(x) calls should be linted
  # expect_lint(
  #   "expect_true('lm' %in% class(x))",
  #   "expect_s3_class\\(x, k\\) is better than expect_equal\\(class\\(x\\), k",
  #   expect_s3_class_linter
  # )
})

test_that("expect_s4_class_linter skips allowed usages", {
  # expect_s4_class doesn't have an inverted version
  expect_lint("expect_true(!is(x, 'class'))", NULL, expect_s4_class_linter())
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint("testthat::expect_s3_class(!is(x, 'class'))", NULL, expect_s4_class_linter())
})

test_that("expect_s4_class blocks simple disallowed usages", {
  expect_lint(
    "expect_true(is(x, 'data.frame'))",
    rex::rex("expect_s4_class(x, k) is better than expect_true(is(x, k))"),
    expect_s4_class_linter()
  )

  # namespace qualification is irrelevant
  expect_lint(
    "testthat::expect_true(methods::is(x, 'SpatialPolygonsDataFrame'))",
    rex::rex("expect_s4_class(x, k) is better than expect_true(is(x, k))"),
    expect_s4_class_linter()
  )
})

skip_if_not_installed("patrick")
local({
  # test for lint errors appropriately raised for all is.<class> calls
  is_classes <- c(
    "data.frame", "factor", "numeric_version",
    "ordered", "package_version", "qr", "table",
    "relistable", "raster", "tclObj", "tkwin", "grob", "unit",
    "mts", "stepfun", "ts", "tskernel"
  )
  patrick::with_parameters_test_that(
    "expect_true(is.<base class>) is caught",
    expect_lint(
      sprintf("expect_true(is.%s(x))", is_class),
      rex::rex("expect_s3_class(x, k) is better than expect_true(is.<k>(x))"),
      expect_s3_class_linter()
    ),
    .test_name = is_classes,
    is_class = is_classes
  )
})
