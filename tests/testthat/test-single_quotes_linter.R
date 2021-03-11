test_that("returns the correct linting", {
  linter <- single_quotes_linter()
  msg <- rex("Only use double-quotes.")

  expect_lint("blah", NULL, linter)
  expect_lint("\"blah\"", NULL, linter)
  expect_lint("\"'blah\"", NULL, linter)
  expect_lint("\"blah'\"", NULL, linter)
  expect_lint("\"blah'\"", NULL, linter)
  expect_lint("\"'blah'\"", NULL, linter)
  expect_lint("'\"'", NULL, linter)
  expect_lint("'\"blah\"'", NULL, linter)

  expect_lint("'blah'", msg, linter)
  expect_lint("fun('blah')", msg, linter)
  expect_lint("{'blah'}", msg, linter)

  expect_lint(
    "
    x = 'test
    '",
    list(
      message = "Only use double-quotes.",
      ranges = list(c(9L, 13L))
    ),
    linter
  )
})
