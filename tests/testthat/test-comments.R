context("comments")
test_that("it detects CI environments", {
  org_value <- Sys.getenv("CI")
  on.exit(Sys.setenv(CI = org_value), add = TRUE)

  Sys.setenv(CI = "true")
  expect_true(in_ci())
  Sys.setenv(CI = "false")
  expect_false(in_ci())
  Sys.setenv(CI = "")
  expect_false(in_ci())
})
