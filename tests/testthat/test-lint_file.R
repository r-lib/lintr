context("Integration tests for lint('some_file')")

# The lints for a given file should be the same regardless of the working
# directory

# Helper function: run assignment_linter on a given file
lint_assignments <- function(filename) {
  lint(filename, linters = list(assignment_linter))
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
    as.data.frame(lints_from_outside)[["line"]], expected_lines
  )
  expect_equal(
    as.data.frame(lints_from_pkg_root), as.data.frame(lints_from_outside)
  )
  expect_equal(
    as.data.frame(lints_from_a_subdir), as.data.frame(lints_from_outside)
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

  lints_with_config_at_pkg_root <- withr::with_dir(
    pkg_path,
    lint_with_config(
      config_path = ".lintr",
      config_string = "exclusions: list('R/jkl.R' = 1)\n",
      filename = file.path("R", "jkl.R")
    )
  )

  lints_with_config_in_source_dir <- withr::with_dir(
    pkg_path,
    lint_with_config(
      config_path = "R/.lintr",
      config_string = "exclusions: list('jkl.R' = 1)\n",
      filename = file.path("R", "jkl.R")
    )
  )

  expect_equal(
    as.data.frame(lints_with_config_at_pkg_root),
    as.data.frame(lints_with_config_in_source_dir)
  )
})
