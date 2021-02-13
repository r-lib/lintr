test_that("test make_linter_from_regex works", {
  linter <- make_linter_from_regex("-", "no_dash_linter", "style", "Silly lint.")()
  expect_lint("a <- 2L", "Silly lint.", linter)
  expect_lint("a = '2-3'", NULL, linter)
})
