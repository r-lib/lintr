test_that("use_lintr works as expected", {
  tmp <- withr::local_tempdir()

  lintr_file <- use_lintr(path = tmp)
  expect_true(file.exists(lintr_file))

  # check that newly created file is in the root directory
  expect_identical(
    normalizePath(lintr_file, winslash = "/"),
    file.path(normalizePath(tmp, winslash = "/"), ".lintr")
  )

  # can't generate if a .lintr already exists
  expect_error(use_lintr(path = tmp), "Found an existing configuration")

  # check that `read_settings()` works with the generated file
  # this can be checked by checking lintr runs successfully
  lints <- lint_dir(tmp)
  expect_length(lints, 0L)
})

test_that("use_lintr with type = full also works", {
  tmp <- withr::local_tempdir()

  # type = "full" also works with read_settings()
  lintr_file <- use_lintr(path = tmp, type = "full")
  expect_true(file.exists(lintr_file))

  # check that newly created file is in the root directory
  expect_identical(
    normalizePath(lintr_file, winslash = "/"),
    file.path(normalizePath(tmp, winslash = "/"), ".lintr")
  )

  lints <- lint_dir(tmp)
  expect_length(lints, 0L)
})

test_that("use_lintr add .lintr to .Rbuildignore for packages", {
  tmp <- withr::local_tempdir()
  tmp_package_dir <- paste0(tmp, "/package")
  package_dir <- test_path("dummy_packages", "package")
  dir.create(tmp_package_dir)
  file.copy(package_dir, tmp, recursive = TRUE)
  setwd(tmp_package_dir)
  lintr_file <- use_lintr()
  expect_true(file.exists(lintr_file))
  expect_snapshot(cat(brio::read_file(file.path(tmp_package_dir, ".Rbuildignore"))))
})
