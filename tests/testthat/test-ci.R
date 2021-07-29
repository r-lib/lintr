test_that("GitHub Actions functionality works", {
  # imitate being on GHA whether or not we are
  withr::with_envvar(c(GITHUB_ACTIONS = "true"), {
    old <- options(lintr.rstudio_source_markers = FALSE)
    on.exit(options(old), add = TRUE)

    writeLines("x <- 1:nrow(y)", tmp <- tempfile())
    on.exit(unlink(tmp))

    l <- lint(tmp)
    expect_output(print(l), "::warning file", fixed = TRUE)
  })
})

test_that("GitHub Actions functionality works in a subdirectory", {
  # imitate being on GHA whether or not we are
  withr::with_envvar(c(GITHUB_ACTIONS = "true"), {
    old <- options(lintr.rstudio_source_markers = FALSE)
    on.exit(options(old), add = TRUE)

    read_settings(NULL)
    pkg_path <- test_path(file.path("dummy_packages", "assignmentLinter"))
    l <- lint_package(
      pkg_path, linters = list(assignment_linter()),
      parse_settings = FALSE, path_prefix = file.path("dummy_packages", "assignmentLinter")
    )
    expect_output(
      print(l),
      paste0("::warning file=dummy_packages/assignmentLinter/R/abc\\.R")
    )
  })
})

test_that("Printing works for Travis", {
  withr::with_envvar(c(GITHUB_ACTIONS = "false", TRAVIS_REPO_SLUG = "test/repo", LINTR_COMMENT_BOT = "true"), {
    old <- options(lintr.rstudio_source_markers = FALSE)
    on.exit(options(old), add = TRUE)

    writeLines("x <- 1:nrow(y)", tmp <- tempfile())
    on.exit(unlink(tmp))

    l <- lint(tmp)

    with_mock(github_comment = function(x, ...) cat(x, "\n"), .env = asNamespace("lintr"), {
      expect_output(print(l), "*warning:*", fixed = TRUE)
    })
  })
})

test_that("Printing works for Wercker", {
  withr::with_envvar(c(GITHUB_ACTIONS = "false", WERCKER_GIT_BRANCH = "test/repo", LINTR_COMMENT_BOT = "true"), {
    old <- options(lintr.rstudio_source_markers = FALSE)
    on.exit(options(old), add = TRUE)

    writeLines("x <- 1:nrow(y)", tmp <- tempfile())
    on.exit(unlink(tmp))

    l <- lint(tmp)

    with_mock(github_comment = function(x, ...) cat(x, "\n"), .env = asNamespace("lintr"), {
      expect_output(print(l), "*warning:*", fixed = TRUE)
    })
  })
})
