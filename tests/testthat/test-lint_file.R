# The lints for a given file should be the same regardless of the working
# directory

# Helper function: run assignment_linter on a given file
lint_assignments <- function(filename) {
  lint(filename, linters = list(assignment_linter()))
}

test_that("lint() results do not depend on the working directory", {

  # a dummy package for use in the test
  pkg_path <- file.path("dummy_packages", "assignmentLinter")

  # put a .lintr in the package root that excludes the first line of `R/jkl.R`
  config_path <- file.path(pkg_path, ".lintr")
  config_string <- "exclusions: list('R/jkl.R' = 1)\n"
  cat(config_string, file = config_path)
  on.exit(unlink(config_path))

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

  expect_equal(
    as.data.frame(lints_from_pkg_root)[["line"]], expected_lines
  )
  expect_equal(
    as.data.frame(lints_from_outside), as.data.frame(lints_from_pkg_root)
  )
  expect_equal(
    as.data.frame(lints_from_a_subdir), as.data.frame(lints_from_pkg_root)
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
  lint_with_config <- function(
    config_path, config_string, filename
  ) {
    cat(config_string, file = config_path)
    on.exit(unlink(config_path))
    lint_assignments(filename)
  }

  # a dummy package for use in the test
  pkg_path <- file.path("dummy_packages", "assignmentLinter")

  # we lint the file <pkg-root>/R/jkl.R using the pkg-root as working directory
  # and
  # - 1) a .lintr config in the package root,
  # - 2) a .lintr config in the source directory R/

  # The second line of jkl.R contains the following assignment lint:
  expected_lines <- "mno = 789"

  lints_with_config_at_pkg_root <- withr::with_dir(
    pkg_path,
    lint_with_config(
      config_path = ".lintr",
      config_string = "exclusions: list('R/jkl.R' = 1)\n",
      filename = file.path("R", "jkl.R")
    )
  )

  lints_with_config_in_r_dir <- withr::with_dir(
    pkg_path,
    lint_with_config(
      config_path = "R/.lintr",
      config_string = "exclusions: list('jkl.R' = 1)\n",
      filename = file.path("R", "jkl.R")
    )
  )

  expect_equal(
    as.data.frame(lints_with_config_at_pkg_root)[["line"]], expected_lines
  )
  expect_equal(
    as.data.frame(lints_with_config_at_pkg_root),
    as.data.frame(lints_with_config_in_r_dir),
    info = paste(
      "lints for a source file should be independent of whether the .lintr",
      "file is in the project-root or the source-file-directory"
    )
  )
})

test_that("lint uses linter names", {
  expect_lint("a = 2", list(linter = "bla"), linters = list(bla = assignment_linter()), parse_settings = FALSE)
})

test_that("lint() results from file or text should be consistent", {
  linters <- list(assignment_linter(), infix_spaces_linter())
  file <- tempfile()
  lines <- c(
    "x<-1",
    "x+1"
  )
  writeLines(lines, file)
  text <- paste0(lines, collapse = "\n")
  file <- normalizePath(file)

  lint_from_file <- lint(file, linters = linters)
  lint_from_lines <- lint(linters = linters, text = lines)
  lint_from_text <- lint(linters = linters, text = text)

  # Remove file before linting to ensure that lint works and do not
  # assume that file exists when both filename and text are supplied.
  unlink(file)
  lint_from_text2 <- lint(file, linters = linters, text = text)

  expect_equal(length(lint_from_file), 2)
  expect_equal(length(lint_from_lines), 2)
  expect_equal(length(lint_from_text), 2)
  expect_equal(length(lint_from_text2), 2)

  expect_equal(lint_from_file, lint_from_text2)

  for (i in seq_along(lint_from_lines)) {
    lint_from_file[[i]]$filename <- ""
    lint_from_lines[[i]]$filename <- ""
    lint_from_text[[i]]$filename <- ""
  }

  expect_equal(lint_from_file, lint_from_lines)
  expect_equal(lint_from_file, lint_from_text)
})

test_that("exclusions work with custom linter names", {
  expect_lint(
    "a = 2 # nolint: bla.",
    NULL,
    linters = list(bla = assignment_linter()),
    parse_settings = FALSE
  )
})

test_that("compatibility warnings work", {
  expect_warning(
    expect_lint(
      "a == NA",
      "Use is.na",
      linters = equals_na_linter
    ),
    regexp = "Passing linters as variables",
    fixed = TRUE
  )

  expect_warning(
    expect_lint(
      "a == NA",
      "Use is.na",
      linters = unclass(equals_na_linter())
    ),
    regexp = "The use of linters of class 'function'",
    fixed = TRUE
  )

  # Trigger compatibility in auto_names()
  expect_warning(
    expect_lint(
      "a == NA",
      "Use is.na",
      linters = list(unclass(equals_na_linter()))
    ),
    fixed = "The use of linters of class 'function'"
  )

  expect_error(
    expect_warning(
      lint("a <- 1\n", linters = function(two, arguments) NULL),
      regexp = "The use of linters of class 'function'",
      fixed = TRUE
    ),
    regexp = "`fun` must be a function taking exactly one argument",
    fixed = TRUE
  )

  expect_error(
    lint("a <- 1\n", linters = "equals_na_linter"),
    regexp = rex("Expected '", anything, "' to be of class 'linter'")
  )
})
