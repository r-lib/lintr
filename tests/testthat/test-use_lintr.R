test_that("use_lintr works as expected", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  lintr_file <- use_lintr(path = tmp)
  expect_true(file.exists(lintr_file))

  # can't generate if a .lintr already exists
  expect_error(use_lintr(path = tmp), "already exists")

  # read_settings() works with the generated file
  expect_silent(read_settings(tmp))
  read_settings(NULL)

  file.remove(lintr_file)
  # type = "full" also works with read_settings()
  use_lintr(path = tmp, type = "full")
  expect_silent(read_settings(tmp))
  read_settings(NULL)

  expect_equal(
    normalizePath(find_config(tmp)),
    normalizePath(lintr_file)
  )
})
