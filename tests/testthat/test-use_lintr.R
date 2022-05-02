test_that("use_lintr works as expected", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  lintr_file <- use_lintr(path = tmp)
  expect_true(file.exists(lintr_file))
  expect_error(use_lintr(path = tmp), "already exists")
  expect_silent(read.dcf(lintr_file))
  expect_silent(read_settings(tmp))
  read_settings(NULL)

  expect_equal(
    normalizePath(find_config(tmp)),
    normalizePath(lintr_file)
  )
})
