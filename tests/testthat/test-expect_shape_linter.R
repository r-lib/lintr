test_that("expect_shape_linter skips allowed usages", {
  linter <- expect_shape_linter()

  expect_no_lint("expect_shape(x, nrow = 4L)", linter)
  expect_no_lint("expect_shape(x, dim = c(2L, 3L))", linter)
  expect_no_lint("expect_equal(length(x), 4L)", linter)
  expect_no_lint("testthat::expect_equal(length(x), 4L)", linter)

  # only check shape functions in the second argument if the first argument is a literal/constant
  #   expectation, because there are legitimate uses of shape functions in argument 2 against dynamic targets
  expect_no_lint("expect_equal(length(x), nrow(y))", linter)

  # expect_shape() doesn't have info= or label= arguments
  expect_no_lint("expect_equal(nrow(x), n, info = 'x should have nrow n')", linter)
  expect_no_lint("expect_equal(ncol(x), n, label = 'x ncol')", linter)
  expect_no_lint("expect_equal(dim(x), d, expected.label = 'target dim')", linter)
})

test_that("expect_shape_linter blocks simple disallowed usages", {
  linter <- expect_shape_linter()
  lint_msg_nrow <- rex::rex("expect_shape(x, nrow = n) is better than expect_equal(nrow(x), n)")
  lint_msg_ncol <- rex::rex("expect_shape(x, ncol = n) is better than expect_equal(ncol(x), n)")
  lint_msg_dim <- rex::rex("expect_shape(x, dim = d) is better than expect_equal(dim(x), d)")

  expect_lint("expect_equal(nrow(x), 2L)", lint_msg_nrow, linter)
  expect_lint("expect_equal(ncol(x), 3L)", lint_msg_ncol, linter)
  expect_lint("expect_equal(dim(x), c(2L, 3L))", lint_msg_dim, linter)
  expect_lint("testthat::expect_equal(nrow(DF), nrow(old))", lint_msg_nrow, linter)
  expect_lint("expect_equal(base::nrow(x), 2L)", lint_msg_nrow, linter)

  # yoda test cases
  expect_lint("expect_equal(2L, nrow(x))", lint_msg_nrow, linter)
  expect_lint("expect_equal(3, ncol(x))", lint_msg_ncol, linter)
  expect_lint("expect_equal(c(2L, 3L), dim(x))", lint_msg_dim, linter)
  expect_lint("expect_equal(2:3, dim(x))", lint_msg_dim, linter)
})

test_that("expect_shape_linter blocks expect_identical usage as well", {
  linter <- expect_shape_linter()

  expect_lint(
    "expect_identical(nrow(x), 2L)",
    rex::rex("expect_shape(x, nrow = n) is better than expect_identical(nrow(x), n)"),
    linter
  )
  expect_lint(
    "expect_identical(dim(x), c(2L, 3L))",
    rex::rex("expect_shape(x, dim = d) is better than expect_identical(dim(x), d)"),
    linter
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      expect_equal(nrow(x), n)
      expect_identical(dim(x), d)
    }"),
    list(
      list("nrow = n.*expect_equal", line_number = 2L),
      list("dim = d.*expect_identical", line_number = 3L)
    ),
    expect_shape_linter()
  )
})
