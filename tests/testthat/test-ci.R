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

  lintr:::read_settings(NULL)
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

test_that("GitHub Actions - linting on error works", {
  # imitate being on GHA whether or not we are
  withr::local_envvar(list(GITHUB_ACTIONS = "true", LINTR_ERROR_ON_LINT = "true"))
  withr::local_options(lintr.rstudio_source_markers = FALSE)
  tmp <- withr::local_tempfile(lines = "x <- 1:nrow(y)")

  l <- lint(tmp)

  local_mocked_bindings(quit = function(...) cat("Tried to quit.\n"))
  expect_output(print(l), "::warning file", fixed = TRUE)
})
