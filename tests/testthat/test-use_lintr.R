test_that("use_lintr works as expected", {
  tmp <- withr::local_tempdir()

  lintr_file <- use_lintr(path = tmp)
  expect_true(file.exists(lintr_file))

  # check that newly created file is in the root directory
  expect_identical(
    normalize_path(lintr_file),
    file.path(normalize_path(tmp), ".lintr")
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
    normalize_path(lintr_file),
    file.path(normalize_path(tmp), ".lintr")
  )

  skip_if_not_installed("cyclocomp") # avoid warning
  lints <- lint_dir(tmp)
  expect_length(lints, 0L)
})

test_that("No .Rbuildignore is created of packages", {
  tmp <- withr::local_tempdir()

  lintr_file <- use_lintr(path = tmp, type = "full")
  expect_false(file.exists(file.path(tmp, ".Rbuildignore")))
})

test_that("No .Rbuildignore is filled outside of packages", {
  tmp <- withr::local_tempdir()
  ignore <- file.path(tmp, ".Rbuildignore")
  file.create(ignore)

  lintr_file <- use_lintr(path = tmp, type = "full")
  expect_identical(readLines(ignore), character())
})

test_that("No .Rbuildignore is filled if matching regex exists", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "DESCRIPTION"))
  ignore <- file.path(tmp, ".Rbuildignore")
  file.create(ignore)
  cat(file = ignore, ".*", sep = "\n")

  lintr_file <- use_lintr(path = tmp, type = "full")
  expect_identical(readLines(ignore), ".*")
})

test_that("use_lintr creates the correct regex", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "DESCRIPTION"))
  ignore <- file.path(tmp, ".Rbuildignore")
  file.create(ignore)
  cat(file = ignore, "^fu$", "^bar$", sep = "\n")

  expect_message({
    lintr_file <- use_lintr(path = tmp, type = "full")
  }, regexp = "Adding .* to .Rbuildignore")
  expect_identical(readLines(ignore), c("^fu$", "^bar$", "^\\.lintr$"))
})

test_that("use_lintr handles missing final new line", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "DESCRIPTION"))
  ignore <- file.path(tmp, ".Rbuildignore")
  file.create(ignore)
  cat(file = ignore, "^fu$\n^bar$")

  expect_message({
    lintr_file <- use_lintr(path = tmp, type = "full")
  }, regexp = "Adding .* to .Rbuildignore")
  expect_identical(readLines(ignore), c("^fu$", "^bar$", "^\\.lintr$"))
})
