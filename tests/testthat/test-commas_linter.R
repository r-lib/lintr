context("commas_linter")

test_that("returns the correct linting", {
  msg_before <- rex("Commas should never have a space before.")
  msg_after <- rex("Commas should always have a space after.")
  linter <- commas_linter()
  expect_is(linter, "linter")

  expect_lint("blah", NULL, linter)

  expect_lint("fun(1, 1)", NULL, linter)

  expect_lint("fun(1,\n  1)", NULL, linter)

  expect_lint("fun(1,\n1)", NULL, linter)

  expect_lint("fun(1\n,\n1)", NULL, linter)

  expect_lint("fun(1\n  ,\n1)", NULL, linter)

  expect_lint("fun(1\n,1)", msg_after, linter)

  expect_lint("fun(1,1)", msg_after, linter)

  expect_lint("\nfun(1,1)", msg_after, linter)

  expect_lint("fun(1 ,1)", list(msg_before, msg_after), linter)

  expect_lint("\"fun(1 ,1)\"", NULL, linter)

  expect_lint("a[1, , 2]", NULL, linter)

  expect_lint("a[1, , 2, , 3]", NULL, linter)
})
