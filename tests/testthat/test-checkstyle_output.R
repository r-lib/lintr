test_that("return lint report as checkstyle xml", {
  lints <- list(
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
  )
  class(lints) <- "lints"
  tmp <- withr::local_tempfile()
  checkstyle_output(lints, tmp)

  # The second line is the checkstyle version, so we ignore it during the
  # check, so we don't have to update the version every release.
  expect_identical(readLines(tmp)[-2L], readLines(test_path("checkstyle.xml"))[-2L])
})
