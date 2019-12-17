context("Integration tests for lint_package")

# Two packages were set up, one (withExclusions) has a .lintr file that
# specifies to exclude the file "R/abc.R" and line 1 from "R/jkl.R"
#
# Aside from the absence of the .lintr file from "withoutExclusions", there are
# no differences between these dummy packages

# When called from inside a package:
# lint_package(".")
# .. should give the same results as when called from outside the package
# with:
# lint_package(path_to_package)

test_that(
  "`lint_package` does not depend on path to pkg - no excluded files", {

  expected_lines <- c(
    # from abc.R
    "abc = 123",
    # from jkl.R
    "jkl = 456", "mno = 789"
  )
  pkg_path <- file.path("dummy_packages", "withoutExclusions")
  lints_from_a_distance <- lint_package(
    pkg_path, linters = list(assignment_linter)
  )

  lints_from_inside <- withr::with_dir(
    pkg_path,
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

  # When excluding the whole of abc.R and the first line of jkl.R:
  pkg_path <- file.path("dummy_packages", "withExclusions")
  expected_lines <- c("mno = 789")
  lints_from_a_distance <- lint_package(
    pkg_path, linters = list(assignment_linter)
  )

  lints_from_inside <- withr::with_dir(
    pkg_path,
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
