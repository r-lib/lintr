context("directory linters")

test_that("lint all files in a directory", {
  the_dir <- "package/vignettes"
  files <- list.files(the_dir)

  lints <- lint_dir(the_dir)
  linted_files <- unique(names(lints))

  expect_s3_class(lints, "lints")
  expect_setequal(linted_files, files)
})

test_that("lint all relevant directories in a package", {
  the_pkg <- "package"
  files <- setdiff(
    list.files(the_pkg, recursive = TRUE),
    c("package.Rproj", "DESCRIPTION", "NAMESPACE")
  )

  lints <- lint_package(the_pkg)
  linted_files <- unique(names(lints))

  # lintr paths contain backslash on windows, list.files uses forward slash.
  linted_files <- gsub("\\", "/", linted_files, fixed = TRUE)

  expect_s3_class(lints, "lints")
  expect_setequal(linted_files, files)
})
