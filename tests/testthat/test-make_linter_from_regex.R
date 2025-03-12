test_that("make_linter_from_regex works", { # nofuzz: assignment
  linter <- lintr:::make_linter_from_regex("-", "style", "Silly lint.")()
  expect_lint("a <- 2L", "Silly lint.", linter)
  expect_no_lint("a = '2-3'", linter)
})
