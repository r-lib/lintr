test_that("expect_type_linter skips allowed usages", {
  # expect_type doesn't have an inverted version
  expect_lint("expect_true(!is.numeric(x))", NULL, expect_type_linter())
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint("testthat::expect_true(!is.numeric(x))", NULL, expect_type_linter)

  # other is.<x> calls are not suitable for expect_type in particular
  expect_lint("expect_true(is.data.frame(x))", NULL, expect_type_linter())

  # expect_type(x, ...) cannot be cleanly used here:
  expect_lint("expect_true(typeof(x) %in% c('builtin', 'closure'))", NULL, expect_type_linter)
})

test_that("expect_type_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_equal(typeof(x), 'double')",
    rex::rex("expect_type(x, t) is better than expect_equal(typeof(x), t)"),
    expect_type_linter
  )

  # expect_identical is treated the same as expect_equal
  expect_lint(
    "testthat::expect_identical(typeof(x), 'language')",
    rex::rex("expect_type(x, t) is better than expect_equal(typeof(x), t)"),
    expect_type_linter
  )

  # different equivalent usage
  expect_lint(
    "expect_true(is.complex(foo(x)))",
    rex::rex("expect_type(x, t) is better than expect_equal(typeof(x), t)"),
    expect_type_linter
  )
})


local({
  # test for lint errors appropriately raised for all is.<type> calls
  is_types <- c(
    "raw", "logical", "integer", "double", "complex", "character", "list",
    "numeric", "function", "primitive", "environment", "pairlist", "promise",
    "language", "call", "name", "symbol", "expression"
  )
  patrick::with_parameters_test_that(
    "expect_type_linter catches expect_true(is.<base type>)",
    expect_lint(
      sprintf("expect_true(is.%s(x))", is_type),
      rex::rex("expect_type(x, t) is better than expect_equal(typeof(x), t)"),
      expect_type_linter
    ),
    .test_name = is_types,
    is_type = is_types
  )
})
