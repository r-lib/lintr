test_that("it uses default settings if none provided", {
  lintr:::read_settings(NULL)

  lapply(ls(settings), function(setting) {
    expect_identical(settings[[setting]], default_settings[[setting]])
  })
})

test_that("it uses option settings if provided", {
  withr::local_options(list(lintr.exclude = "test"))

  lintr:::read_settings(NULL)

  expect_identical(settings$exclude, "test")
})

test_that("it uses config settings in same directory if provided", {
  test_dir <- tempdir()
  file <- withr::local_tempfile(tmpdir = test_dir)
  local_config(test_dir, 'exclude: "test"')

  lintr:::read_settings(file)

  lapply(setdiff(ls(settings), "exclude"), function(setting) {
    expect_identical(settings[[setting]], default_settings[[setting]])
  })

  expect_identical(settings$exclude, "test")
})

test_that("it uses config home directory settings if provided", {
  path <- withr::local_tempdir()
  home_path <- withr::local_tempdir()
  file <- withr::local_tempfile(tmpdir = path)
  local_config(home_path, 'exclude: "test"')

  withr::with_envvar(c(HOME = home_path), lintr:::read_settings(file))

  lapply(setdiff(ls(settings), "exclude"), function(setting) {
    expect_identical(settings[[setting]], default_settings[[setting]])
  })

  expect_identical(settings$exclude, "test")
})

test_that("it errors if the config file does not end in a newline", {
  f <- withr::local_tempfile()
  cat("linters: linters_with_defaults(closed_curly_linter = NULL)", file = f)
  withr::local_options(list(lintr.linter_file = f))
  expect_error(lintr:::read_settings("foo"), "Malformed config file")
})

test_that("rot utility works as intended", {
  expect_identical(lintr:::rot(letters), c(letters[14L:26L], LETTERS[1L:13L]))
})

test_that("logical_env utility works as intended", {
  test_env <- "LINTR_TEST_LOGICAL_ENV_"
  sym_set_env <- function(key, value) do.call(Sys.setenv, setNames(list(value), key))
  old <- Sys.getenv(test_env, unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv(test_env) else sym_set_env(test_env, old))

  sym_set_env(test_env, "true")
  expect_true(lintr:::logical_env(test_env))

  sym_set_env(test_env, "F")
  expect_false(lintr:::logical_env(test_env))

  sym_set_env(test_env, "")
  expect_null(lintr:::logical_env(test_env))

  Sys.unsetenv(test_env)
  expect_null(lintr:::logical_env(test_env))
})

# fixing #774
test_that("linters_with_defaults doesn't break on very long input", {
  expect_named(
    linters_with_defaults(
      defaults = list(),
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
    ),
    "undesirable_function_linter"
  )
})

test_that("it has a smart default for encodings", {
  lintr:::read_settings(NULL)
  expect_identical(settings$encoding, "UTF-8")

  proj_file <- test_path("dummy_projects", "project", "metropolis-hastings-rho.R")
  pkg_file <- test_path("dummy_packages", "cp1252", "R", "metropolis-hastings-rho.R")

  expect_identical(
    normalizePath(find_rproj_at(find_rproj_or_package(proj_file)), winslash = "/"),
    normalizePath(test_path("dummy_projects", "project", "project.Rproj"), winslash = "/")
  )
  expect_identical(
    normalizePath(find_package(pkg_file), winslash = "/"),
    normalizePath(test_path("dummy_packages", "cp1252"), winslash = "/")
  )

  expect_identical(lintr:::find_default_encoding(proj_file), "ISO8859-1")
  expect_identical(lintr:::find_default_encoding(pkg_file), "ISO8859-1")

  lintr:::read_settings(proj_file)
  expect_identical(settings$encoding, "ISO8859-1")

  lintr:::read_settings(pkg_file)
  expect_identical(settings$encoding, "ISO8859-1")
})
