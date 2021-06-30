test_that("it uses default settings if none provided", {

  read_settings(NULL)

  lapply(ls(settings), function(setting) {
    expect_identical(settings[[setting]], default_settings[[setting]])
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
    expect_identical(settings[[setting]], default_settings[[setting]])
  })

  expect_equal(settings$exclude, "test")
})

test_that("it uses config home directory settings if provided", {

  path <- tempfile()
  home_path <- tempfile()
  dir.create(path)
  dir.create(home_path)
  file <- tempfile(tmpdir = path)
  config_file <- file.path(home_path, ".lintr")
  writeLines("exclude: \"test\"", config_file)
  on.exit({
    unlink(file)
    unlink(config_file)
    unlink(path)
    unlink(home_path)
  },
  add = TRUE)

  withr::with_envvar(c(HOME = home_path), read_settings(file))

  lapply(setdiff(ls(settings), "exclude"), function(setting) {
    expect_identical(settings[[setting]], default_settings[[setting]])
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

test_that("rot utility works as intended", {
  expect_equal(lintr:::rot(letters), c(letters[14:26], LETTERS[1:13]))
})

test_that("logical_env utility works as intended", {
  test_env <- "LINTR_TEST_LOGICAL_ENV_"
  sym_set_env <- function(key, value) do.call(Sys.setenv, setNames(list(value), key))
  old <- Sys.getenv(test_env, unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv(test_env) else sym_set_env(test_env, old))

  sym_set_env(test_env, "true")
  expect_equal(lintr:::logical_env(test_env), TRUE)

  sym_set_env(test_env, "F")
  expect_equal(lintr:::logical_env(test_env), FALSE)

  sym_set_env(test_env, "")
  expect_null(lintr:::logical_env(test_env))

  Sys.unsetenv(test_env)
  expect_null(lintr:::logical_env(test_env))
})

# fixing #774
test_that("with_defaults doesn't break on very long input", {
  expect_equal(
    names(with_defaults(
      default = list(),
      lintr::undesirable_function_linter(c(
        detach = paste(
          "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
          "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
          "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
          "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
          "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
          "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
          "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
        )
      ))
    )),
    "lintr::undesirable_function_linter"
  )
})

test_that("it has a smart default for encodings", {
  read_settings(NULL)
  expect_equal(settings$encoding, "UTF-8")

  proj_file <- file.path("dummy_projects", "project", "metropolis-hastings-rho.R")
  pkg_file <- file.path("dummy_packages", "cp1252", "R", "metropolis-hastings-rho.R")

  expect_equal(
    normalizePath(find_rproj(proj_file), winslash = "/"),
    normalizePath(file.path("dummy_projects", "project", "project.Rproj"), winslash = "/")
  )
  expect_equal(
    normalizePath(find_package(pkg_file), winslash = "/"),
    normalizePath(file.path("dummy_packages", "cp1252"), winslash = "/")
  )

  expect_equal(find_default_encoding(proj_file), "ISO8859-1")
  expect_equal(find_default_encoding(pkg_file), "ISO8859-1")

  read_settings(proj_file)
  expect_equal(settings$encoding, "ISO8859-1")

  read_settings(pkg_file)
  expect_equal(settings$encoding, "ISO8859-1")
})
