test_that("GitHub Actions functionality works", {
  # imitate being on GHA whether or not we are
  withr::local_envvar(list(GITHUB_ACTIONS = "true"))
  withr::local_options(lintr.rstudio_source_markers = FALSE)
  tmp <- withr::local_tempfile(lines = "x <- 1:nrow(y)")

  l <- lint(tmp)
  expect_output(print(l), "::warning file", fixed = TRUE)
})

test_that("GitHub Actions functionality works in a subdirectory", {
  # imitate being on GHA whether or not we are
  pkg_path <- test_path("dummy_packages", "assignmentLinter")
  withr::local_envvar(list(GITHUB_ACTIONS = "true"))
  withr::local_options(lintr.rstudio_source_markers = FALSE, lintr.github_annotation_project_dir = pkg_path)

  read_settings(NULL)
  l <- lint_package(
    pkg_path,
    linters = list(assignment_linter()),
    parse_settings = FALSE
  )
  expect_output(
    print(l),
    paste0("::warning file=", file.path(pkg_path, "R(/|\\\\)abc\\.R"))
  )
})

test_that("Printing works for Travis", {
  withr::local_envvar(list(GITHUB_ACTIONS = "false", TRAVIS_REPO_SLUG = "test/repo", LINTR_COMMENT_BOT = "true"))
  withr::local_options(lintr.rstudio_source_markers = FALSE)
  tmp <- withr::local_tempfile(lines = "x <- 1:nrow(y)")

  l <- lint(tmp)

  mockery::stub(print.lints, "github_comment", function(x, ...) cat(x, "\n"))
  expect_output(print(l), "*warning:*", fixed = TRUE)
})

test_that("Printing works for Wercker", {
  withr::local_envvar(list(GITHUB_ACTIONS = "false", WERCKER_GIT_BRANCH = "test/repo", LINTR_COMMENT_BOT = "true"))
  withr::local_options(lintr.rstudio_source_markers = FALSE)
  tmp <- withr::local_tempfile(lines = "x <- 1:nrow(y)")

  l <- lint(tmp)

  mockery::stub(print.lints, "github_comment", function(x, ...) cat(x, "\n"))
  expect_output(print(l), "*warning:*", fixed = TRUE)
})
