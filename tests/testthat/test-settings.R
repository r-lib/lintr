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

  path <- tempdir()
  file <- tempfile(tmpdir = path)
  config_file <- file.path(path, ".lintr")
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

test_that("it errors if the config file does not end in a newline", {

  f <- tempfile()
  cat("linters: with_defaults(closed_curly_linter = NULL)", file = f)
  old <- options()
  on.exit(options(old))
  options("lintr.linter_file" = f)
  expect_error(read_settings("foo"), "Malformed config file")
})

test_that("with_defaults works as expected", {
  # test capturing unnamed args
  defaults <- with_defaults(assignment_linter)
  # assignment_linter is in defaults, so output doesn't change
  expect_equal(names(defaults), names(with_defaults()))
})
