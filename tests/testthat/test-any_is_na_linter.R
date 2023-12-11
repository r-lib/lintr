test_that("any_is_na_linter skips allowed usages", {
  linter <- any_is_na_linter()

  expect_lint("x <- any(y)", NULL, linter)

  expect_lint("y <- is.na(z)", NULL, linter)

  # extended usage of ... arguments to any is not covered
  expect_lint("any(is.na(y), b)", NULL, linter)
  expect_lint("any(b, is.na(y))", NULL, linter)

  # negation shouldn't list
  expect_lint("any(!is.na(x))", NULL, linter)
  expect_lint("any(!is.na(foo(x)))", NULL, linter)
})

test_that("any_is_na_linter blocks simple disallowed usages", {
  linter <- any_is_na_linter()
  lint_message <- rex::rex("anyNA(x) is better than any(is.na(x)).")

  expect_lint("any(is.na(x))", lint_message, linter)
  expect_lint("any(is.na(foo(x)))", lint_message, linter)
  # na.rm doesn't really matter for this since is.na can't return NA
  expect_lint("any(is.na(x), na.rm = TRUE)", lint_message, linter)
  # also catch nested usage
  expect_lint("foo(any(is.na(x)))", lint_message, linter)
})

test_that("lints vectorize", {
  lint_message <- rex::rex("anyNA(x) is better than any(is.na(x)).")

  expect_lint(
    trim_some("{
      any(is.na(foo(x)))
      any(is.na(y), na.rm = TRUE)
    }"),
    list(
      list(lint_message, line_number = 2L),
      list(lint_message, line_number = 3L)
    ),
    any_is_na_linter()
  )
})
