test_that("return lint report as checkstyle xml", {
  lints <- structure(
    list(
      Lint(
        filename = "test_file",
        line_number = 1L,
        column_number = 2L,
        type = "error",
        line = "a line",
        message = "foo"
      ),
      Lint(
        filename = "test_file",
        line_number = 2L,
        column_number = 1L,
        type = "style",
        line = "another line",
        message = "bar"
      ),
      Lint(
        filename = "test_file2",
        line_number = 1L,
        column_number = 1L,
        type = "warning",
        line = "yet another line",
        message = "baz"
      )
    ),
    class = "lints"
  )
  tmp <- tempfile()
  checkstyle_output(lints, tmp)

  expect_equal(readLines(tmp), readLines("checkstyle.xml"))
})
