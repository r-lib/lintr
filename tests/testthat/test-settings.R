context("read_settings")
test_that("it uses default settings if none provided", {

  read_settings(NULL)

  lapply(ls(settings), function(setting) {
    expect_equal(settings[[setting]], default_settings[[setting]])
  })
})

test_that("it uses option settings if provided", {

  old_opts <- options("lintr.exclude" = "test")
  on.exit(options(old_opts))

  read_settings(NULL)

  expect_equal(settings$exclude, "test")
})

test_that("it uses config settings in same directory if provided", {

  dir <- tempdir()
  file <- tempfile(tmpdir = dir)
  config_file <- file.path(dir, ".lintr")
  writeLines("exclude: \"test\"", config_file)
  on.exit({
    unlink(file)
    unlink(config_file)
  },
  add = TRUE)

  read_settings(file)

  lapply(setdiff(ls(settings), "exclude"), function(setting) {
    expect_equal(settings[[setting]], default_settings[[setting]])
  })

  expect_equal(settings$exclude, "test")
})
