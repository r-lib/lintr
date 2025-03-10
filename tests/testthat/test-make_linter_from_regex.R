test_that("make_linter_from_regex works", { # nofuzz
  linter <- lintr:::make_linter_from_regex("-", "style", "Silly lint.")()
  expect_lint("a <- 2L", "Silly lint.", linter)
  expect_lint("a = '2-3'", NULL, linter)
})
