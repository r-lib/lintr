test_that("expect_named_linter doesn't raise false positive lints", {
  expect_lint("expect_equal(nrow(x), 4L)", NULL, expect_named_linter())
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint("testthat::expect_equal(nrow(x), 4L)", NULL, expect_named_linter())
})

test_that("expect_named_linter skips allowed usages", {
  # colnames(), rownames(), and dimnames() tests are not equivalent
  expect_lint("expect_equal(colnames(x), 'a')", NULL, expect_named_linter())
  expect_lint("expect_equal(rownames(x), 'a')", NULL, expect_named_linter())
  expect_lint("expect_equal(dimnames(x), 'a')", NULL, expect_named_linter())

  # only check the first argument. yoda tests in the second argument will be
  #   missed, but there are legitimate uses of names() in argument 2
  expect_lint("expect_equal(colnames(x), names(y))", NULL, expect_named_linter())
})

test_that("expect_named_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_equal(names(x), 'a')",
    rex::rex("expect_named(x, n) is better than expect_equal(names(x), n)"),
    expect_named_linter()
  )

  expect_lint(
    "testthat::expect_equal(names(DF), names(old))",
    rex::rex("expect_named(x, n) is better than expect_equal(names(x), n)"),
    expect_named_linter()
  )

  expect_lint(
    "expect_equal('a', names(x))",
    rex::rex("expect_named(x, n) is better than expect_equal(names(x), n)"),
    expect_named_linter()
  )
})

test_that("expect_named_linter blocks expect_identical usage as well", {
  expect_lint(
    "expect_identical(names(x), 'a')",
    rex::rex("expect_named(x, n) is better than expect_identical(names(x), n)"),
    expect_named_linter()
  )
})
