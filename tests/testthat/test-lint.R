test_that("Lint() errors on invalid input", {
  expect_error(Lint("dummy.R", ranges = list(c(1L, NA_integer_))), rex::rex("`ranges` must not contain NAs."))
})
