context("Integration tests for `lint_package`")

# When called from inside a package:
# lint_package(".")
# .. should give the same results as when called from outside the package
# with:
# lint_package(path_to_package)

# Template packages for use in testing are stored in
# `tests/testthat/dummy_packages/<pkgName>`
# These packages should not have a .lintr file:  Hardcoding a .lintr in a
# dummy package throws problems during `R CMD check` (they are flagged as
# hidden files, but can't be added to RBuildIgnore since they should be
# available during `R CMD check` tests)

# We copy the package structure in "tests/testthat/dummy_packages/<pkgName>"
# to a temp location, so that a .lintr file can be added to the package while
# testing.

make_temp_package_copy <- function(dummy_package) {
  # Creates a temporary copy of the input package, and returns the filepath for
  # that copy
  #
  # Temp location: <temp_directory>/tempPackage<random_string>/pkgName
  package_name <- basename(dummy_package)
  parent_of_copy <- tempfile(pattern = "tempPackage")
  dir.create(parent_of_copy)
  file.copy(dummy_package, parent_of_copy, recursive = TRUE)
  file.path(parent_of_copy, package_name)
}

test_that(
  "`lint_package` does not depend on path to pkg - no excluded files", {

  package_name <- "withoutExclusions"
  copied_package_path <- make_temp_package_copy(
    file.path("dummy_packages", package_name)
  )

  expected_lines <- c(
    # from abc.R
    "abc = 123",
    # from jkl.R
    "jkl = 456", "mno = 789"
  )
  lints_from_a_distance <- lint_package(
    copied_package_path, linters = list(assignment_linter)
  )

  lints_from_inside <- withr::with_dir(
    copied_package_path,
    lint_package(".", linters = list(assignment_linter))
  )

  expect_equal(
    as.data.frame(lints_from_a_distance)[["line"]], expected_lines
  )
  expect_equal(
    as.data.frame(lints_from_a_distance),
    as.data.frame(lints_from_inside),
    info = paste(
      "lint_package() finds the same lints when no files are excluded,",
      "regardless of path"
    )
  )
})

test_that(
  "`lint_package` does not depend on path to pkg - with excluded files", {
  # Since excluded regions can be specified in two ways
  # list(
  #   filename = line_numbers, # approach 1
  #   filename                 # approach 2
  # ),
  # the test checks both approaches

  pkg_name <- "withoutExclusions"
  copied_package_path <- make_temp_package_copy(
    file.path("dummy_packages", pkg_name)
  )

  # Add a .lintr that excludes the whole of `abc.R` and the first line of
  # `jkl.R`:
  cat(
    "exclusions: list('R/abc.R', 'R/jkl.R' = 1)\n",
    file = file.path(copied_package_path, ".lintr")
  )
  expected_lines <- c("mno = 789")

  lints_from_a_distance <- lint_package(
    copied_package_path, linters = list(assignment_linter)
  )

  lints_from_inside <- withr::with_dir(
    copied_package_path,
    lint_package(".", linters = list(assignment_linter))
  )

  expect_equal(
    as.data.frame(lints_from_a_distance)[["line"]], expected_lines
  )
  expect_equal(
    as.data.frame(lints_from_a_distance),
    as.data.frame(lints_from_inside),
    info = paste(
      "lint_package() finds the same lints when files / lines are excluded,",
      "regardless of path"
    )
  )
})
