test_that("expect_type_linter skips allowed usages", {
  linter <- expect_type_linter()

  # expect_type doesn't have an inverted version
  expect_lint("expect_true(!is.numeric(x))", NULL, linter)
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint("testthat::expect_true(!is.numeric(x))", NULL, linter)

  # other is.<x> calls are not suitable for expect_type in particular
  expect_lint("expect_true(is.data.frame(x))", NULL, linter)

  # expect_type(x, ...) cannot be cleanly used here:
  expect_lint("expect_true(typeof(x) %in% c('builtin', 'closure'))", NULL, linter)

  # expect_type() doesn't have info= or label= arguments
  expect_lint("expect_equal(typeof(x), t, info = 'x should have type t')", NULL, linter)
  expect_lint("expect_equal(typeof(x), t, label = 'x type')", NULL, linter)
  expect_lint("expect_equal(typeof(x), t, expected.label = 'type')", NULL, linter)
  expect_lint("expect_true(is.double(x), info = 'x should be double')", NULL, linter)
})

test_that("expect_type_linter blocks simple disallowed usages", {
  linter <- expect_type_linter()

  expect_lint(
    "expect_equal(typeof(x), 'double')",
    rex::rex("expect_type(x, t) is better than expect_equal(typeof(x), t)"),
    linter
  )

  # expect_identical is treated the same as expect_equal
  expect_lint(
    "testthat::expect_identical(typeof(x), 'language')",
    rex::rex("expect_type(x, t) is better than expect_identical(typeof(x), t)"),
    linter
  )

  # different equivalent usage
  expect_lint(
    "expect_true(is.complex(foo(x)))",
    rex::rex("expect_type(x, t) is better than expect_true(is.<t>(x))"),
    linter
  )

  # yoda test with clear expect_type replacement
  expect_lint(
    "expect_equal('integer', typeof(x))",
    rex::rex("expect_type(x, t) is better than expect_equal(typeof(x), t)"),
    linter
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
      rex::rex("expect_type(x, t) is better than expect_true(is.<t>(x))"),
      expect_type_linter()
    ),
    .test_name = is_types,
    is_type = is_types
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      expect_true(is.integer(x))
      expect_equal(typeof(x), 'double')
    }"),
    list(
      list("expect_true", line_number = 2L),
      list("expect_equal", line_number = 3L)
    ),
    expect_type_linter()
  )
})
