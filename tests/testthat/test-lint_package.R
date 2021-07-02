# When called from inside a package:
# lint_package(".") # nolint
# .. should give the same results as when called from outside the package
# with:
# lint_package(path_to_package) # nolint

# Template packages for use in testing are stored in
# tests/testthat/dummy_packages/<pkgName>
# These packages should not have a .lintr file:  Hardcoding a .lintr in a
# dummy package throws problems during `R CMD check` (they are flagged as
# hidden files, but can't be added to RBuildIgnore since they should be
# available during `R CMD check` tests)

test_that(
  "`lint_package` does not depend on path to pkg - no excluded files", {

  # This dummy package does not have a .lintr file, so no files / lines should
  # be excluded from analysis
  pkg_path <- file.path("dummy_packages", "assignmentLinter")

  expected_lines <- c(
    # from abc.R
    "abc = 123",
    # from jkl.R
    "jkl = 456", "mno = 789"
  )
  read_settings(NULL)
  lints_from_outside <- lint_package(
    pkg_path, linters = list(assignment_linter()), parse_settings = FALSE
  )
  lints_from_pkg_root <- withr::with_dir(
    pkg_path,
    lint_package(".", linters = list(assignment_linter()), parse_settings = FALSE)
  )
  lints_from_a_subdir <- withr::with_dir(
    file.path(pkg_path, "R"),
    lint_package("..", linters = list(assignment_linter()), parse_settings = FALSE)
  )

  expect_equal(
    as.data.frame(lints_from_outside)[["line"]], expected_lines
  )
  expect_equal(
    as.data.frame(lints_from_outside),
    as.data.frame(lints_from_pkg_root),
    info = paste(
      "lint_package() finds the same lints from pkg-root as from outside a pkg",
      "(no .lintr config present)"
    )
  )
  expect_equal(
    as.data.frame(lints_from_outside),
    as.data.frame(lints_from_a_subdir),
    info = paste(
      "lint_package() finds the same lints from a subdir as from outside a pkg",
      "(no .lintr config present)"
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

  pkg_path <- file.path("dummy_packages", "assignmentLinter")
  config_path <- file.path(pkg_path, ".lintr")

  # Add a .lintr that excludes the whole of `abc.R` and the first line of
  # `jkl.R` (and remove it on finishing this test)
  cat(
    "exclusions: list('R/abc.R', 'R/jkl.R' = 1)\n",
    file = config_path
  )
  on.exit(unlink(config_path))

  expected_lines <- c("mno = 789")
  lints_from_outside <- lint_package(
    pkg_path, linters = list(assignment_linter())
  )
  lints_from_pkg_root <- withr::with_dir(
    pkg_path,
    lint_package(".", linters = list(assignment_linter()))
  )
  lints_from_a_subdir <- withr::with_dir(
    file.path(pkg_path, "R"),
    lint_package("..", linters = list(assignment_linter()))
  )

  expect_equal(
    as.data.frame(lints_from_outside)[["line"]], expected_lines
  )
  expect_equal(
    as.data.frame(lints_from_outside),
    as.data.frame(lints_from_pkg_root),
    info = paste(
      "lint_package() finds the same lints from pkg-root as from outside a pkg",
      "(.lintr config present)"
    )
  )
  expect_equal(
    as.data.frame(lints_from_outside),
    as.data.frame(lints_from_a_subdir),
    info = paste(
      "lint_package() finds the same lints from a subdir as from outside a pkg",
      "(.lintr config present)"
    )
  )
})

test_that("lint_package returns early if no package is found", {

  temp_pkg <- tempfile("dir")
  dir.create(temp_pkg)
  on.exit(unlink(temp_pkg, recursive = TRUE))

  expect_warning(l <- lint_package(temp_pkg), "Didn't find any R package", fixed = TRUE)
  expect_null(l)

  # ignore a folder named DESCRIPTION, #702
  file.copy(file.path("dummy_packages", "desc_dir_pkg"), temp_pkg, recursive = TRUE)

  expect_warning(
    lint_package(file.path(temp_pkg, "desc_dir_pkg", "DESCRIPTION", "R")),
    "Didn't find any R package", fixed = TRUE
  )
})
