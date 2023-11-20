test_that("rep_len_linter skips allowed usages", {
  linter <- rep_len_linter()

  # only catch length.out usages
  expect_lint("rep(x, y)", NULL, linter)
  expect_lint("rep(1:10, 2)", NULL, linter)
  expect_lint("rep(1:10, 10:1)", NULL, linter)

  # usage of each is not compatible with rep_len; see ?rep.
  expect_lint("rep(x, each = 4, length.out = 50)", NULL, linter)
  # each is implicitly the 4th positional argument. a very strange usage
  #   (because length.out is ignored), but doesn't hurt to catch it
  expect_lint("rep(a, b, length.out = c, d)", NULL, linter)
  # ditto for implicit length.out=
  expect_lint("rep(a, b, c, d)", NULL, linter)
})

test_that("rep_len_linter blocks simple disallowed usages", {
  linter <- rep_len_linter()
  lint_msg <- rex::rex("Use rep_len(x, n) instead of rep(x, length.out = n).")

  # only catch length.out usages
  expect_lint("rep(x, length.out = 4L)", lint_msg, linter)

  # implicit times= argument; length.out has priority over times= (see ?rep),
  #   so we still lint since it's as if times= is not supplied.
  # (notice here that the base behavior is odd -- one might expect output like
  #   head(rep(1:10, 10:1), 50), but instead we get rep(1:10, length.out = 50))
  expect_lint("rep(1:10, 10:1, length.out = 50)", lint_msg, linter)

  # ditto for explicit times= argument
  expect_lint("rep(1:10, times = 10:1, length.out = 50)", lint_msg, linter)

  # implicit usage in third argument
  expect_lint("rep(1:10, 10:1, 50)", lint_msg, linter)
})

test_that("vectorized lints work", {
  lint_msg <- rex::rex("Use rep_len(x, n) instead of rep(x, length.out = n).")

  expect_lint(
    trim_some("{
      rep(x, y)
      rep(1:10, length.out = 50)
      rep(x, each = 4, length.out = 50)
      rep(x, length.out = 50)
    }"),
    list(
      list(lint_msg, line_number = 3L),
      list(lint_msg, line_number = 5L)
    ),
    rep_len_linter()
  )
})
