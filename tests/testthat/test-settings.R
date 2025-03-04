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
  local_config('exclude: "test"', test_dir)

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
  local_config('exclude: "test"', home_path)

  withr::with_envvar(c(HOME = home_path), lintr:::read_settings(file))

  lapply(setdiff(ls(settings), "exclude"), function(setting) {
    expect_identical(settings[[setting]], default_settings[[setting]])
  })

  expect_identical(settings$exclude, "test")
})

test_that("it uses system config directory settings if provided", {
  path <- withr::local_tempdir()
  config_parent_path <- withr::local_tempdir("config")
  config_path <- file.path(config_parent_path, "R", "lintr")
  dir.create(config_path, recursive = TRUE)
  file <- withr::local_tempfile(tmpdir = path)
  local_config('exclude: "test"', config_path, filename = "config")

  withr::with_envvar(c(R_USER_CONFIG_DIR = config_parent_path), lintr:::read_settings(file))

  lapply(setdiff(ls(settings), "exclude"), function(setting) {
    expect_identical(settings[[setting]], default_settings[[setting]])
  })

  expect_identical(settings$exclude, "test")
})

test_that("read_config_file() warns if the config file does not end in a newline", {
  .lintr <- withr::local_tempfile()
  withr::local_options(lintr.linter_file = .lintr)
  withr::local_dir(withr::local_tempdir())

  # cat() not writeLines() to ensure no trailing \n
  cat("linters: linters_with_defaults(brace_linter = NULL)", file = .lintr)
  writeLines("a <- 1", "aaa.R")
  expect_warning(lint_dir(), "Warning encountered while loading config", fixed = TRUE)
})

test_that("it gives informative errors if the config file contains errors", {
  .lintr <- withr::local_tempfile(lines = c(
    "linters: linters_with_defaults(",
    "    brace_linter = NULL,",
    "  )"
  ))
  withr::local_options(lintr.linter_file = .lintr)
  withr::local_dir(withr::local_tempdir())

  writeLines("a <- 1", "aaa.R")
  expect_error(lint_dir(), "Error from config setting `linters`", fixed = TRUE)
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
  withr::local_options(list(lintr.linter_file = tempfile()))

  lintr:::read_settings(NULL)
  expect_identical(settings$encoding, "UTF-8")

  proj_file <- test_path("dummy_projects", "project", "cp1252.R")
  pkg_file <- test_path("dummy_packages", "cp1252", "R", "cp1252.R")

  expect_identical(
    normalize_path(find_rproj_at(find_package(proj_file, allow_rproj = TRUE))),
    normalize_path(test_path("dummy_projects", "project", "project.Rproj"))
  )
  expect_identical(
    normalize_path(find_package(pkg_file)),
    normalize_path(test_path("dummy_packages", "cp1252"))
  )

  expect_identical(lintr:::find_default_encoding(proj_file), "ISO8859-1")
  expect_identical(lintr:::find_default_encoding(pkg_file), "ISO8859-1")

  lintr:::read_settings(proj_file)
  expect_identical(settings$encoding, "ISO8859-1")

  lintr:::read_settings(pkg_file)
  expect_identical(settings$encoding, "ISO8859-1")
})

test_that("validate_config_file() detects improperly-formed settings", {
  .lintr <- withr::local_tempfile()
  withr::local_options(lintr.linter_file = .lintr)
  withr::local_dir(withr::local_tempdir())

  writeLines("asdf: 1", .lintr)
  expect_warning(lint_dir(), "Found unused settings in config", fixed = TRUE)

  writeLines("a=1", "aaa.R")
  writeLines(c('exclusions: list("aaa.R")', "asdf: 1"), .lintr)
  expect_warning(lint_dir(), "Found unused settings in config", fixed = TRUE)

  encoding_error_msg <- "Setting `encoding` should be a character string"

  writeLines("encoding: FALSE", .lintr)
  expect_error(lint_dir(), encoding_error_msg, fixed = TRUE)

  writeLines("encoding: NA_character_", .lintr)
  expect_error(lint_dir(), encoding_error_msg, fixed = TRUE)

  writeLines('encoding: c("a", "b")', .lintr)
  expect_error(lint_dir(), encoding_error_msg, fixed = TRUE)

  exclude_error_msg <- "Setting `exclude` should be a single regular expression"

  writeLines("exclude: FALSE", .lintr)
  expect_error(lint_dir(), exclude_error_msg, fixed = TRUE)

  writeLines(c('exclusions: list("aaa.R")', "exclude: FALSE"), .lintr)
  expect_error(lint_dir(), exclude_error_msg, fixed = TRUE)

  writeLines('exclude: "("', .lintr)
  expect_error(lint_dir(), exclude_error_msg, fixed = TRUE)

  writeLines("linters: list(1)", .lintr)
  expect_error(lint_dir(), "Setting `linters` should be a list of linters", fixed = TRUE)

  writeLines("linters: list(assignment_linter(), 1)", .lintr)
  expect_error(lint_dir(), "Setting `linters` should be a list of linters", fixed = TRUE)

  name_exclusion_error_msg <- "Unnamed entries of setting `exclusions` should be strings"

  writeLines("exclusions: list(1L)", .lintr)
  expect_error(lint_dir(), name_exclusion_error_msg, fixed = TRUE)

  writeLines('exclusions: list("aaa.R", 1L)', .lintr)
  expect_error(lint_dir(), name_exclusion_error_msg, fixed = TRUE)

  writeLines("exclusions: list(letters)", .lintr)
  expect_error(lint_dir(), name_exclusion_error_msg, fixed = TRUE)

  writeLines("exclusions: list(NA_character_)", .lintr)
  expect_error(lint_dir(), name_exclusion_error_msg, fixed = TRUE)

  exclusion_error_msg <- "Named entries of setting `exclusions` should designate line numbers"

  writeLines('exclusions: list(aaa.R = "abc")', .lintr)
  expect_error(lint_dir(), exclusion_error_msg, fixed = TRUE)

  writeLines("exclusions: list(aaa.R = NA_integer_)", .lintr)
  expect_error(lint_dir(), exclusion_error_msg, fixed = TRUE)

  writeLines('exclusions: list(aaa.R = list("abc"))', .lintr)
  expect_error(lint_dir(), exclusion_error_msg, fixed = TRUE)

  writeLines("exclusions: list(aaa.R = list(NA_integer_))", .lintr)
  expect_error(lint_dir(), exclusion_error_msg, fixed = TRUE)

  writeLines('exclusions: list(aaa.R = list(assignment_linter = "abc"))', .lintr)
  expect_error(lint_dir(), exclusion_error_msg, fixed = TRUE)

  writeLines("exclusions: list(aaa.R = list(assignment_linter = NA_integer_))", .lintr)
  expect_error(lint_dir(), exclusion_error_msg, fixed = TRUE)
})

test_that("exclusions can be a character vector", {
  withr::local_dir(withr::local_tempdir())
  # exclusions are relative to dirname(.lintr), so must create it here
  .lintr <- withr::local_tempfile(tmpdir = getwd())
  withr::local_options(lintr.linter_file = .lintr)

  writeLines('exclusions: "aaa.R"', .lintr)
  writeLines("a<-1", "aaa.R")
  writeLines("b<-1", "bbb.R")
  expect_length(lint_dir(linters = infix_spaces_linter()), 1L)

  writeLines('exclusions: c("aaa.R", "bbb.R")', .lintr)
  expect_length(lint_dir(linters = infix_spaces_linter()), 0L)
})

test_that("lines Inf means 'all lines'", {
  withr::local_dir(withr::local_tempdir())
  # exclusions are relative to dirname(.lintr), so must create it here
  .lintr <- withr::local_tempfile(tmpdir = getwd())
  withr::local_options(lintr.linter_file = .lintr)

  writeLines("exclusions: list(aaa.R = Inf)", .lintr)
  writeLines("a<-1", "aaa.R")
  expect_length(lint_dir(linters = infix_spaces_linter()), 0L)

  writeLines("exclusions: list(aaa.R = list(infix_spaces_linter = Inf))", .lintr)
  # exclude infix_spaces_linter, include assignment_linter()
  writeLines("a=1", "aaa.R")
  expect_length(lint_dir(linters = list(assignment_linter(), infix_spaces_linter())), 1L)
})

test_that("read_config_file() bubbles up warnings helpfully, without erroring (#2253)", {
  .lintr <- withr::local_tempfile(lines = 'linters: list(backport_linter("2.0.0"))')
  withr::local_options(lintr.linter_file = .lintr)
  withr::local_dir(withr::local_tempdir())

  writeLines("a <- 1", "aaa.R")
  expect_warning(
    lint_dir(),
    'Depending on an R version older than "3.0.0" is not recommended'
  )
})

test_that("perl-only regular expressions are accepted in config", {
  .lintr <- withr::local_tempfile(lines = 'exclude: "# (?<=a)b"')
  withr::local_options(lintr.linter_file = .lintr)
  withr::local_dir(withr::local_tempdir())

  writeLines("a <- 1", "aaa.R")
  expect_silent(lint("aaa.R"))
})

test_that("settings can be put in a sub-directory", {
  withr::local_dir(withr::local_tempdir())

  dir.create(".settings")
  .lintr <- ".settings/.lintr.R"
  writeLines("linters <- list(line_length_linter(10L))", .lintr)

  dir.create("R")
  writeLines("abcdefghijklmnopqrstuvwxyz=1", "R/a.R")

  writeLines(c("Package: foo", "Version: 0.1"), "DESCRIPTION")

  withr::local_options(lintr.linter_file = .lintr)
  expect_length(lint_package(), 1L)
})
