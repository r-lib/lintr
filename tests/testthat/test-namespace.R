test_that("is_s3_generic", {
  func <- function(x) {
    print(x)
    UseMethod("func")
  }

  expect_true(is_s3_generic(func))
})
