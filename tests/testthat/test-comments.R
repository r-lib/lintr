clear_ci_info <- function() {
  withr::local_envvar(
    c(
      "JENKINS_URL" = NA_character_,
      "GIT_URL" = NA_character_,
      "GIT_URL_1" = NA_character_,
      "CHANGE_ID" = NA_character_,
      "GIT_COMMIT" = NA_character_
    ),
    .local_envir = parent.frame()
  )
}

test_that("it detects CI environments", {
  clear_ci_info()
  Sys.setenv(TRAVIS_REPO_SLUG = "foo/bar")
  expect_true(lintr:::in_ci())
  Sys.setenv(TRAVIS_REPO_SLUG = "")
  expect_false(lintr:::in_ci())
})

test_that("it returns NULL if GIT_URL is not on github", {
  clear_ci_info()
  Sys.setenv(
    JENKINS_URL = "https://jenkins.example.org/",
    GIT_URL = "https://example.com/user/repo.git",
    CHANGE_ID = "123"
  )
  expect_false(lintr:::in_ci())
})


test_that("it returns NULL for Jenkins PR build info when git URL is missing", {
  clear_ci_info()
  expect_null(lintr:::jenkins_build_info())
})

test_that("it determines Jenkins PR build info", {
  clear_ci_info()
  Sys.setenv(
    JENKINS_URL = "https://jenkins.example.org/",
    GIT_URL = "https://github.com/user/repo.git",
    CHANGE_ID = "123"
  )
  expect_true(lintr:::in_ci())

  expect_identical(lintr:::ci_build_info(), list(
    user = "user",
    repo = "repo",
    pull = "123",
    commit = NULL
  ))

  Sys.unsetenv(c("JENKINS_URL", "GIT_URL", "CHANGE_ID"))
  expect_false(lintr:::in_ci())
})

test_that("it determines Jenkins commit build info", {
  clear_ci_info()
  Sys.setenv(
    JENKINS_URL = "https://jenkins.example.org/",
    GIT_URL_1 = "https://github.com/user/repo.git",
    GIT_COMMIT = "abcde"
  )

  expect_true(lintr:::in_ci())
  expect_identical(lintr:::ci_build_info(), list(
    user = "user",
    repo = "repo",
    pull = NULL,
    commit = "abcde"
  ))
})
