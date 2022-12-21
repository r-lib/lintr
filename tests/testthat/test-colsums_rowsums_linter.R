test_that("colsums_rowsums_linter skips allowed usages", {
  linter <- colsums_rowsums_linter()

  expect_lint("apply(x, 1, prod)", NULL, linter)

  expect_lint("apply(x, 1, function(i) sum(i[i > 0]))", NULL, linter)

  # sum as FUN argument
  expect_lint("apply(x, 1, f, sum)", NULL, linter)

  # mean() with named arguments other than na.rm is skipped because they are not
  # implemented in colMeans() or rowMeans()
  expect_lint("apply(x, 1, mean, trim = 0.2)", NULL, linter)
})


test_that("colsums_rowsums_linter simple disallowed usages", {
  linter <- colsums_rowsums_linter()
  lint_message <- rex::rex("colSums")

  expect_lint("apply(x, 1, sum)", lint_message, linter)

  expect_lint("apply(x, MARGIN = 1, FUN = sum)", lint_message, linter)

  expect_lint("apply(x, 2, sum)", lint_message, linter)

  expect_lint("apply(x, 2:4, sum)", lint_message, linter)

  # Works with extra args in sum()
  expect_lint("apply(x, 1, sum, na.rm = TRUE)", lint_message, linter)

  lint_message <- rex::rex("colMeans")

  expect_lint("apply(x, 1, mean)", lint_message, linter)

  expect_lint("apply(x, MARGIN = 1, FUN = mean)", lint_message, linter)

  expect_lint("apply(x, 2, mean)", lint_message, linter)

  expect_lint("apply(x, 2:4, mean)", lint_message, linter)

  # Works with extra args in mean()
  expect_lint("apply(x, 1, mean, na.rm = TRUE)", lint_message, linter)

})

test_that("colsums_rowsums_linter works with multiple lints in a single expression", {
  linter <- colsums_rowsums_linter()

  expect_lint(
    "rbind(
      apply(x, 1, sum),
      apply(y, 1, mean)
    )",
    list(
      rex::rex("colSums"),
      rex::rex("colMeans")
    ),
    linter
  )

})
