# The lints for a given file should be the same regardless of the working
# directory

test_that("lint() results do not depend on the working directory", {
  # Helper function: run assignment_linter on a given file
  lint_assignments <- function(filename) {
    lint(filename, linters = list(assignment_linter()))
  }

  # a dummy package for use in the test
  pkg_path <- test_path("dummy_packages", "assignmentLinter")

  # put a .lintr in the package root that excludes the first line of `R/jkl.R`
  local_config("exclusions: list('R/jkl.R' = 1)", pkg_path)

  # linting the `R/jkl.R` should identify the following assignment lint on the
  # second line of the file
  expected_lines <- "mno = 789"

  # lint the file from:
  # - outside the package
  # - at the package root
  # - in the package's R/ directory

  lints_from_outside <- lint_assignments(
    file.path(pkg_path, "R", "jkl.R")
  )
  lints_from_pkg_root <- withr::with_dir(
    pkg_path,
    lint_assignments(file.path("R", "jkl.R"))
  )
  lints_from_a_subdir <- withr::with_dir(
    file.path(pkg_path, "R"),
    lint_assignments("jkl.R")
  )

  expect_identical(
    as.data.frame(lints_from_pkg_root)[["line"]],
    expected_lines
  )
  expect_identical(
    as.data.frame(lints_from_outside),
    as.data.frame(lints_from_pkg_root)
  )
  expect_identical(
    as.data.frame(lints_from_a_subdir),
    as.data.frame(lints_from_pkg_root)
  )
})

# The lints for a given file should be the same regardless of where the .lintr
# file is positioned (file-exclusions in the .lintr should be relative to the
# directory containing the .lintr)

test_that("lint() results do not depend on the position of the .lintr", {
  # .lintr config files for lint(filepath) are looked for in:
  # - the same directory as filepath
  # - the project directory
  # - the user's home directory
  lint_with_config <- function(config_string, config_dir, filename) {
    local_config(config_string, config_dir)
    lint(filename, linters = assignment_linter())
  }

  # a dummy package for use in the test
  pkg_path <- test_path("dummy_packages", "assignmentLinter")

  # we lint the file <pkg-root>/R/jkl.R using the pkg-root as working directory
  # and
  # - 1) a .lintr config in the package root,
  # - 2) a .lintr config in the source directory R/

  # The second line of jkl.R contains the following assignment lint:
  expected_lines <- "mno = 789"

  lints_with_config_at_pkg_root <- withr::with_dir(
    pkg_path,
    lint_with_config(
      config_string = "exclusions: list('R/jkl.R' = 1)",
      config_dir = ".",
      filename = file.path("R", "jkl.R")
    )
  )

  lints_with_config_in_r_dir <- withr::with_dir(
    pkg_path,
    lint_with_config(
      config_string = "exclusions: list('jkl.R' = 1)",
      config_dir = "R",
      filename = file.path("R", "jkl.R")
    )
  )

  expect_identical(
    as.data.frame(lints_with_config_at_pkg_root)[["line"]], expected_lines
  )
  expect_identical(
    as.data.frame(lints_with_config_at_pkg_root),
    as.data.frame(lints_with_config_in_r_dir),
    info = paste(
      "lints for a source file should be independent of whether the .lintr",
      "file is in the project-root or the source-file-directory"
    )
  )
})

test_that("lint uses linter names", {
  expect_lint(
    "a = 2",
    list(linter = "bla"),
    linters = list(bla = assignment_linter()),
    parse_settings = FALSE
  )
})

test_that("lint() results from file or text should be consistent", {
  linters <- list(assignment_linter(), infix_spaces_linter())
  lines <- c("x<-1", "x+1")
  file <- withr::local_tempfile(lines = lines)
  text <- paste(lines, collapse = "\n")
  file <- normalize_path(file)

  lint_from_file <- lint(file, linters = linters)
  lint_from_lines <- lint(linters = linters, text = lines)
  lint_from_text <- lint(linters = linters, text = text)

  # Remove file before linting to ensure that lint works and do not
  # assume that file exists when both filename and text are supplied.
  expect_identical(unlink(file), 0L)
  lint_from_text2 <- lint(file, linters = linters, text = text)

  expect_length(lint_from_file, 2L)
  expect_length(lint_from_lines, 2L)
  expect_length(lint_from_text, 2L)
  expect_length(lint_from_text2, 2L)

  expect_identical(lint_from_file, lint_from_text2)

  for (i in seq_along(lint_from_lines)) {
    lint_from_file[[i]]$filename <- ""
    lint_from_lines[[i]]$filename <- ""
    lint_from_text[[i]]$filename <- ""
  }

  expect_identical(lint_from_file, lint_from_lines)
  expect_identical(lint_from_file, lint_from_text)
})

test_that("exclusions work with custom linter names", {
  expect_no_lint(
    "a = 2 # nolint: bla.",
    linters = list(bla = assignment_linter()),
    parse_settings = FALSE
  )
})

test_that("old compatibility usage errors", {
  error_msg <- rex::rex("Expected `", anything, "` to be a function of class <linter>")

  expect_error(
    expect_lint(
      "a == NA",
      "Use is.na",
      linters = equals_na_linter
    ),
    error_msg
  )

  expect_error(
    expect_lint(
      "a = 42",
      "Use <-",
      linters = assignment_linter
    ),
    error_msg
  )

  # Also within `linters_with_defaults()` (#1725)
  expect_error(
    expect_lint(
      "a = 42",
      "Use <-",
      linters = linters_with_defaults(assignment_linter)
    ),
    error_msg
  )

  expect_error(
    expect_lint(
      "a == NA",
      "Use is.na",
      linters = unclass(equals_na_linter())
    ),
    error_msg
  )

  # Trigger compatibility in auto_names()
  expect_error(
    expect_lint(
      "a == NA",
      "Use is.na",
      linters = list(unclass(equals_na_linter()))
    ),
    error_msg
  )

  expect_error(
    lint("a <- 1\n", linters = function(two, arguments) NULL),
    error_msg
  )

  expect_error(
    lint("a <- 1\n", linters = "equals_na_linter"),
    error_msg
  )
})

test_that("Linters throwing an error give a helpful error", {
  tmp_file <- withr::local_tempfile(lines = "a <- 1")
  lintr_error_msg <- "a broken linter"
  linter <- function() Linter(function(source_expression) cli_abort(lintr_error_msg))
  # NB: Some systems/setups may use e.g. symlinked files when creating under tempfile();
  #   we don't care much about that, so just check basename()
  expect_error(lint(tmp_file, linter()), lintr_error_msg, fixed = TRUE)
  expect_error(lint(tmp_file, list(broken_linter = linter())), lintr_error_msg, fixed = TRUE)
})

test_that("Linter() input is validated", {
  expect_error(Linter(1L), "`fun` must be a function taking exactly one argument", fixed = TRUE)
  expect_error(Linter(function(a, b) TRUE), "`fun` must be a function taking exactly one argument", fixed = TRUE)
})

test_that("typo in argument name gives helpful error", {
  expect_error(lint("xxx", litners = identity), "Found unknown arguments in `...`: `litners`")
})


test_that("gitlab_output() writes expected report", {
  skip_if_not_installed("jsonlite")

  tmpfile <- withr::local_tempfile()

  # zero lints: we expect an empty json array
  gitlab_output(lint(text = "", linters = infix_spaces_linter()), filename = tmpfile)
  expect_match(
    readLines(tmpfile),
    R"(\s*\[\s*\]\s*)"
  )

  # single lint
  gitlab_output(lint(text = "x<-1", linters = infix_spaces_linter()), filename = tmpfile)
  expect_identical(
    jsonlite::read_json(tmpfile),
    list(list(
      description = "Put spaces around all infix operators.",
      check_name = "infix_spaces_linter",
      fingerprint = "eb7cc117e8616bd8170fe6aa29e8b0ae849ac6c7",
      location = list(path = "<text>", lines = list(begin = 1L)),
      severity = "info"
    ))
  )

  # two lints
  gitlab_output(lint(text = c("x<-1", "y<-1"), linters = infix_spaces_linter()), filename = tmpfile)
  expect_identical(
    jsonlite::read_json(tmpfile),
    list(list(
      description = "Put spaces around all infix operators.",
      check_name = "infix_spaces_linter",
      fingerprint = "eb7cc117e8616bd8170fe6aa29e8b0ae849ac6c7",
      location = list(path = "<text>", lines = list(begin = 1L)),
      severity = "info"
    ), list(
      description = "Put spaces around all infix operators.",
      check_name = "infix_spaces_linter",
      fingerprint = "c20bd2090d08e3a5c12d670f5763ad43d233fe05",
      location = list(path = "<text>", lines = list(begin = 2L)),
      severity = "info"
    ))
  )
})

test_that("explicit parse_settings=TRUE works for inline data", {
  withr::local_dir(tempdir())
  .lintr <- withr::local_tempfile(lines = "linters: list(assignment_linter())")
  withr::local_options(list(lintr.linter_file = .lintr))

  lint_str <- "a=1\n" # assignment lints, but not infix_spaces
  foo.R <- withr::local_tempfile(lines = lint_str)

  expect_length(lint(foo.R, parse_settings = TRUE), 1L)
  expect_length(lint(text = lint_str, parse_settings = TRUE), 1L)
  expect_length(lint(lint_str, parse_settings = TRUE), 1L)
})
