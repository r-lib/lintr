test_that("lint all files in a directory", {
  the_dir <- file.path("dummy_packages", "package", "vignettes")
  files <- list.files(the_dir)

  lints <- lint_dir(the_dir, parse_settings = FALSE)
  linted_files <- unique(names(lints))

  expect_s3_class(lints, "lints")
  expect_setequal(linted_files, files)
})

test_that("lint all relevant directories in a package", {
  the_pkg <- file.path("dummy_packages", "package")
  files <- setdiff(
    list.files(the_pkg, recursive = TRUE),
    c("package.Rproj", "DESCRIPTION", "NAMESPACE")
  )

  read_settings(NULL)
  lints <- lint_package(the_pkg, parse_settings = FALSE)
  linted_files <- unique(names(lints))

  # lintr paths contain backslash on windows, list.files uses forward slash.
  linted_files <- gsub("\\", "/", linted_files, fixed = TRUE)

  expect_s3_class(lints, "lints")
  expect_setequal(linted_files, files)

  # Code coverage is not detected for default_linters.
  # We want to ensure that object_name_linter uses namespace_imports correctly.
  # assignment_linter is needed to cause a lint in all vignettes.
  linters <- list(assignment_linter(), object_name_linter())
  read_settings(NULL)
  lints <- lint_package(the_pkg, linters = linters, parse_settings = FALSE)
  linted_files <- unique(names(lints))

  # lintr paths contain backslash on windows, list.files uses forward slash.
  linted_files <- gsub("\\", "/", linted_files, fixed = TRUE)

  expect_s3_class(lints, "lints")
  expect_setequal(linted_files, files)
})

test_that("respects directory exclusions", {
  the_dir <- tempfile()
  dir.create(the_dir, recursive = TRUE)
  on.exit(unlink(the_dir, recursive = TRUE))

  the_excluded_dir <- file.path(the_dir, "exclude-me")
  dir.create(the_excluded_dir)

  file.copy("default_linter_testcode.R", the_dir)
  file.copy("default_linter_testcode.R", the_excluded_dir)
  file.copy("default_linter_testcode.R", file.path(the_excluded_dir, "bad2.R"))

  lints <- lint_dir(the_dir, exclusions = "exclude-me")
  linted_files <- unique(names(lints))
  expect_length(linted_files, 1L)
  expect_identical(linted_files, "default_linter_testcode.R")

  lints_norm <- lint_dir(the_dir, exclusions = "exclude-me", relative_path = FALSE)
  linted_files <- unique(names(lints_norm))
  expect_length(linted_files, 1L)
  expect_identical(linted_files, normalizePath(file.path(the_dir, "default_linter_testcode.R")))

})

test_that("respect directory exclusions from settings", {
  the_dir <- tempfile()
  dir.create(the_dir, recursive = TRUE)
  on.exit(unlink(the_dir, recursive = TRUE))

  the_excluded_dir <- file.path(the_dir, "exclude-me")
  dir.create(the_excluded_dir)

  file.copy("default_linter_testcode.R", the_dir)
  file.copy("default_linter_testcode.R", the_excluded_dir)
  file.copy("default_linter_testcode.R", file.path(the_excluded_dir, "bad2.R"))
  cat("exclusions:\n  'exclude-me'\n", file = file.path(the_dir, ".lintr"))

  lints <- lint_dir(the_dir)
  linted_files <- unique(names(lints))
  expect_length(linted_files, 1L)
})
