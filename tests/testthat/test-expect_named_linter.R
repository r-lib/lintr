test_that("expect_named_linter skips allowed usages", {
  linter <- expect_named_linter()

  # colnames(), rownames(), and dimnames() tests are not equivalent
  expect_lint("expect_equal(colnames(x), 'a')", NULL, linter)
  expect_lint("expect_equal(rownames(x), 'a')", NULL, linter)
  expect_lint("expect_equal(dimnames(x), 'a')", NULL, linter)

  expect_lint("expect_equal(nrow(x), 4L)", NULL, linter)
  # NB: also applies to tinytest, but it's sufficient to test testthat
  expect_lint("testthat::expect_equal(nrow(x), 4L)", NULL, linter)

  # only check the first argument. yoda tests in the second argument will be
  #   missed, but there are legitimate uses of names() in argument 2
  expect_lint("expect_equal(colnames(x), names(y))", NULL, linter)
})

test_that("expect_named_linter blocks simple disallowed usages", {
  linter <- expect_named_linter()
  lint_msg <- rex::rex("expect_named(x, n) is better than expect_equal(names(x), n)")

  expect_lint("expect_equal(names(x), 'a')", lint_msg, linter)
  expect_lint("testthat::expect_equal(names(DF), names(old))", lint_msg, linter)
  expect_lint("expect_equal('a', names(x))", lint_msg, linter)
})

test_that("expect_named_linter blocks expect_identical usage as well", {
  expect_lint(
    "expect_identical(names(x), 'a')",
    rex::rex("expect_named(x, n) is better than expect_identical(names(x), n)"),
    expect_named_linter()
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      expect_equal(names(x), nm)
      expect_identical(names(x), nm)
    }"),
    list(
      list("expect_equal", line_number = 2L),
      list("expect_identical", line_number = 3L)
    ),
    expect_named_linter()
  )
})
