test_that("is_s3_generic", {
  func <- function(x) {
    print(x)
    UseMethod("func")
  }

  expect_true(is_s3_generic(func))
})

test_that("is_s3_generic doesn't error for namespace-qualified calls", {
  func <- function(...) {
    pkg::call()
  }

  expect_warning(result <- is_s3_generic(func), NA)
  expect_false(result)
})
