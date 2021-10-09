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
  pkg_path <- file.path("dummy_packages", "assignmentLinter")
  withr::with_envvar(c(GITHUB_ACTIONS = "true"), {
    old <- options(
      lintr.rstudio_source_markers = FALSE,
      lintr.github_annotation_project_dir = pkg_path
    )
    on.exit(options(old), add = TRUE)

    read_settings(NULL)
    l <- lint_package(
      pkg_path, linters = list(assignment_linter()),
      parse_settings = FALSE
    )
    expect_output(
      print(l),
      paste0("::warning file=", file.path(pkg_path, "R(/|\\\\)abc\\.R"))
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
