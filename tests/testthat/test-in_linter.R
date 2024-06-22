test_that("in_linter catches unnecessary OR chains", {
  linter <- in_linter()
  lint_msg <- rex::rex("%in%")

  expect_lint(
    "x == 'a' | x == 'b'",
    lint_msg,
    linter
  )

  # Works with short-circuit operator
  expect_lint(
    "x == 'a' || x == 'b'",
    lint_msg,
    linter
  )

  # Works with yoda tests
  expect_lint(
    "'a' == x || 'b' == x",
    lint_msg,
    linter
  )

  # Works with 'semi-yoda' tests
  expect_lint(
    "x == 'a' || 'b' == x",
    lint_msg,
    linter
  )

  # Works with longer chains
  expect_lint(
    "x == 'a' | x == 'b' | x == 'c'",
    lint_msg,
    linter
  )
})

test_that("common in_linter negative cases", {
  linter <- in_linter()

  expect_lint(
    "x == 'a' | y == 'b'",
    NULL,
    linter
  )

  expect_lint(
    "x == 'a' || y == 'b'",
    NULL,
    linter
  )
})
