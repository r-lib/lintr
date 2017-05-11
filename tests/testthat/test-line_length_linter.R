context("line_length_linter")

test_that("returns the correct linting", {
  msg <- rex("Lines should not be more than 80 characters")
  linter <- line_length_linter(80)
  expect_is(linter, "linter")

  expect_lint("blah", NULL, linter)

  expect_lint("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    NULL,
    linter)

  expect_lint("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    "Lines should not be more than 80 characters",
    linter)

  expect_lint(
    paste0("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n",
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
    list(msg, msg),
    linter)

  expect_lint("aaaaaaaaaaaaaaaaaaaa", NULL, line_length_linter(20))

  expect_lint("aaaaaaaaaaaaaaaaaaaab",
    rex("Lines should not be more than 20 characters"),
    line_length_linter(20))
})
