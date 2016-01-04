context("commas_linter")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, commas_linter)

  expect_lint("fun(1, 1)", NULL, commas_linter)

  expect_lint("fun(1,\n  1)", NULL, commas_linter)

  expect_lint("fun(1,\n1)", NULL, commas_linter)

  expect_lint("fun(1\n,\n1)", NULL, commas_linter)

  expect_lint("fun(1\n  ,\n1)", NULL, commas_linter)

  expect_lint("fun(1\n,1)",
    rex("Commas should always have a space after."),
    commas_linter)

  expect_lint("fun(1,1)",
    rex("Commas should always have a space after."),
    commas_linter)

  expect_lint("\nfun(1,1)",
    rex("Commas should always have a space after."),
    commas_linter)

  expect_lint("fun(1 ,1)",
    list(
      rex("Commas should never have a space before."),
      rex("Commas should always have a space after.")
      ),
    commas_linter)

  expect_lint("\"fun(1 ,1)\"",
    NULL,
    commas_linter)
})
