context("single_quotes_linter")

test_that("returns the correct linting", {
  msg <- rex("Only use double-quotes.")
  linter <- single_quotes_linter()
  expect_is(linter, "linter")

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
})
