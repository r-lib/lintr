context("Test that all CI builds fail when a failing test is present")

test_that("This test will always fail", {
  expect_true(FALSE, info = "all CI builds fail on a failing test")
})
