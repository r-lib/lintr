context("object_multiple_dots_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    object_multiple_dots_linter)

  expect_lint("variable.name.test",
     rex("Words within variable and function names should be separated by '_' rather than '.'."),
    object_multiple_dots_linter)
})
test_that("variables from attached external packages are ignored", {
  expect_lint("print.data.frame",
    NULL,
    object_multiple_dots_linter)

  expect_lint("row.names.data.frame",
    NULL,
    object_multiple_dots_linter)
})
