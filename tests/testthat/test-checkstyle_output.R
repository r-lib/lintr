test_that("return lint report as checkstyle xml", {
  lints <- structure(
    list(
      Lint(filename = "test_file",
        line_number = 1,
        column_number = 2,
        type = "error",
        line = "a line",
        message = "foo"),
      Lint(filename = "test_file",
        line_number = 2,
        column_number = 1,
        type = "style",
        line = "another line",
        message = "bar"),
      Lint(filename = "test_file2",
        line_number = 1,
        column_number = 1,
        type = "warning",
        line = "yet another line",
        message = "baz")
      ),
    class = "lints")
  tmp <- tempfile()
  checkstyle_output(lints, tmp)

  expect_equal(readLines(tmp), readLines("checkstyle.xml"))
})
