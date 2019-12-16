context("Integration tests for lint_package")

make_temp_package <- function(pkgname, path) {
  utils::package.skeleton(name = pkgname, path = path)
}

test_that("lint_package does not depend on how the path is specified", {
  # When called from inside a package:
  # lint_package(".")
  # .. should give the same results (modulo file prefixes) as when called from
  # outside the package with:
  # lint_package(path_to_package)

  # Since excluded regions can be specified in two ways
  # list(filename = line_numbers, filename), the test checks both approaches

  td <- tempdir()
  pkg_path <- tempfile(pattern = "tempPackage")
  pkg_name <- basename(pkg_path)
  utils::package.skeleton(name = pkg_name, path = td)
  cat(
    "abc = 123\nghi <- 456\n", file = file.path(pkg_path, "R", "abc.R")
  )
  cat(
    "jkl = 456\nmno = 789\n", file = file.path(pkg_path, "R", "jkl.R")
  )

  # When including all files:
  expected_lines <- c("abc = 123", "jkl = 456", "mno = 789")
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

  # When excluding the whole of abc.R and the first line of jkl.R:
  cat(
    "exclusions: list('R/abc.R', 'R/jkl.R' = 1)\n",
    file = file.path(pkg_path, ".lintr")
  )
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
      "lint_package() finds the same lints when files are excluded, regardless",
      "of path"
    )
  )

  unlink(pkg_path)
})
