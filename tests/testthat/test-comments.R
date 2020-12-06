test_that("it detects CI environments", {
  org_value <- Sys.getenv("CI")
  on.exit(Sys.setenv(CI = org_value), add = TRUE)

  Sys.setenv(TRAVIS_REPO_SLUG = "foo/bar")
  expect_true(in_ci())
  Sys.setenv(TRAVIS_REPO_SLUG = "")
  expect_false(in_ci())
})
