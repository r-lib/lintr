test_that("any_is_na_linter skips allowed usages", {
  expect_lint("x <- any(y)", NULL, any_is_na_linter())

  expect_lint("y <- is.na(z)", NULL, any_is_na_linter())

  # extended usage of ... arguments to any is not covered
  expect_lint("any(is.na(y), b)", NULL, any_is_na_linter())
  expect_lint("any(b, is.na(y))", NULL, any_is_na_linter())
})

test_that("any_is_na_linter blocks simple disallowed usages", {
  lint_message <- rex::rex("anyNA(x) is better than any(is.na(x)).")
  expect_lint("any(is.na(x))", lint_message, any_is_na_linter())

  expect_lint("any(is.na(foo(x)))", lint_message, any_is_na_linter())

  # na.rm doesn't really matter for this since is.na can't return NA
  expect_lint("any(is.na(x), na.rm = TRUE)", lint_message, any_is_na_linter())

  # also catch nested usage
  expect_lint("foo(any(is.na(x)))", lint_message, any_is_na_linter())
})
