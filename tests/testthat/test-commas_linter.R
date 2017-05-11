context("commas_linter")

test_that("returns the correct linting", {
  msg_before <- rex("Commas should never have a space before.")
  msg_after <- rex("Commas should always have a space after.")
  linter <- commas_linter()
  expect_is(linter, "linter")

  expect_lint("blah", NULL, commas_linter)

  expect_lint("fun(1, 1)", NULL, commas_linter)

  expect_lint("fun(1,\n  1)", NULL, commas_linter)

  expect_lint("fun(1,\n1)", NULL, commas_linter)

  expect_lint("fun(1\n,\n1)", NULL, commas_linter)

  expect_lint("fun(1\n  ,\n1)", NULL, commas_linter)

  expect_lint("fun(1\n,1)", msg_after, commas_linter)

  expect_lint("fun(1,1)", msg_after, commas_linter)

  expect_lint("\nfun(1,1)", msg_after, commas_linter)

  expect_lint("fun(1 ,1)", list(msg_before, msg_after), commas_linter)

  expect_lint("\"fun(1 ,1)\"", NULL, commas_linter)

  expect_lint("a[1, , 2]", NULL, commas_linter)

  expect_lint("a[1, , 2, , 3]", NULL, commas_linter)
})
