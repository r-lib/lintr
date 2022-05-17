test_that("use_lintr works as expected", {
  tmp <- withr::local_tempdir()

  lintr_file <- use_lintr(path = tmp)
  expect_true(file.exists(lintr_file))

  # can't generate if a .lintr already exists
  expect_error(use_lintr(path = tmp), "Found an existing configuration")

  # read_settings() works with the generated file
  expect_silent(read_settings(tmp))
  read_settings(NULL)

  expect_equal(
    normalizePath(find_config(tmp)),
    normalizePath(lintr_file)
  )
})

test_that("use_lintr with type = full also works", {
  tmp <- withr::local_tempdir()

  # type = "full" also works with read_settings()
  use_lintr(path = tmp, type = "full")
  expect_silent(read_settings(tmp))
  read_settings(NULL)
})
