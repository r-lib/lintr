test_that("GitHub Actions functionality works", {
  withr::local_envvar(list(GITHUB_ACTIONS = "true", IN_PKGDOWN = "false"))
  withr::local_options(lintr.rstudio_source_markers = FALSE)
  tmp <- withr::local_tempfile(lines = "x <- 1:nrow(y)")

  l <- lint(tmp)
  expect_output(print(l), "::warning file", fixed = TRUE)
})

test_that("GitHub Actions functionality works in a subdirectory", {
  pkg_path <- test_path("dummy_packages", "assignmentLinter")
  withr::local_envvar(list(GITHUB_ACTIONS = "true"))
  withr::local_options(lintr.rstudio_source_markers = FALSE, lintr.github_annotation_project_dir = pkg_path)

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

patrick::with_parameters_test_that(
  "GitHub Actions - error on lint works",
  {
    withr::local_envvar(list(GITHUB_ACTIONS = "true", IN_PKGDOWN = "", LINTR_ERROR_ON_LINT = env_var_value))
    withr::local_options(lintr.rstudio_source_markers = FALSE)
    tmp <- withr::local_tempfile(lines = "x <- 1:nrow(y)")

    l <- lint(tmp)

    local_mocked_bindings(quit = \(...) cat("Tried to quit.\n"))
    expect_output(print(l), "::warning file", fixed = TRUE)
  },
  env_var_value = list("T", "true")
)

patrick::with_parameters_test_that(
  "GitHub Actions - env var for error on lint is converted to logical",
  {
    withr::local_envvar(list(GITHUB_ACTIONS = "true", LINTR_ERROR_ON_LINT = env_var_value))
    withr::local_options(lintr.rstudio_source_markers = FALSE)
    tmp <- withr::local_tempfile(lines = "x <- 1:nrow(y)")

    l <- lint(tmp)

    expect_output(print(l), "::warning file", fixed = TRUE)
  },
  env_var_value = list("", "F", NA, NULL)
)

test_that("GitHub Actions log is skipped in pkgdown websites", {
  withr::local_envvar(list(GITHUB_ACTIONS = "true", IN_PKGDOWN = "true"))
  withr::local_options(lintr.rstudio_source_markers = FALSE)
  tmp <- withr::local_tempfile(lines = "x <- 1:nrow(y)")

  l <- lint(tmp, linters = seq_linter())
  expect_output(print(l), "warning: [seq_linter]", fixed = TRUE)
})
