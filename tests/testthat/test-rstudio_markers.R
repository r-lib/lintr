context("rstudio_source_markers")
test_that("it returns markers which match lints", {
  with_mock(`rstudioapi::callFun` = function(...) return(list(...)),
    lint1 <- structure(
      list(
        Lint(filename = "test_file",
          line_number = 1,
          column_number = 2,
          type = "error",
          line = "a line",
          message = "hi")
        ),
      class = "lints"
      ),
    marker1 <- rstudio_source_markers(lint1),

    expect_equal(marker1$name, "lintr"),
    expect_equal(marker1$markers[[1]]$type, lint1[[1]]$type),
    expect_equal(marker1$markers[[1]]$file, lint1[[1]]$filename),
    expect_equal(marker1$markers[[1]]$line, lint1[[1]]$line_number),
    expect_equal(marker1$markers[[1]]$column, lint1[[1]]$column_number),
    expect_equal(marker1$markers[[1]]$message, lint1[[1]]$message),

    lint2 <- structure(
      list(
        Lint(filename = "test_file",
          line_number = 1,
          column_number = 2,
          type = "error",
          line = "a line",
          message = "hi"),
        Lint(filename = "test_file2",
          line_number = 10,
          column_number = 5,
          type = "warning",
          message = "test a message")
        ),
      class = "lints"
      ),

    marker2 <- rstudio_source_markers(lint2),

    expect_equal(marker2$name, "lintr"),
    expect_equal(marker2$markers[[1]]$type, lint2[[1]]$type),
    expect_equal(marker2$markers[[1]]$file, lint2[[1]]$filename),
    expect_equal(marker2$markers[[1]]$line, lint2[[1]]$line_number),
    expect_equal(marker2$markers[[1]]$column, lint2[[1]]$column_number),
    expect_equal(marker2$markers[[1]]$message, lint2[[1]]$message),

    expect_equal(marker2$name, "lintr"),
    expect_equal(marker2$markers[[2]]$type, lint2[[2]]$type),
    expect_equal(marker2$markers[[2]]$file, lint2[[2]]$filename),
    expect_equal(marker2$markers[[2]]$line, lint2[[2]]$line_number),
    expect_equal(marker2$markers[[2]]$column, lint2[[2]]$column_number),
    expect_equal(marker2$markers[[2]]$message, lint2[[2]]$message)
  )})

test_that("it prepends the package path if it exists", {
  with_mock(`rstudioapi::callFun` = function(...) return(list(...)),
    lint3 <- structure(
      list(
        Lint(filename = "test_file",
          line_number = 1,
          column_number = 2,
          type = "error",
          line = "a line",
          message = "hi")
        ),
      class = "lints",
      path = "test"
      ),
    marker3 <- rstudio_source_markers(lint3),
    expect_equal(marker3$name, "lintr"),
    expect_equal(marker3$basePath, "test"), # nolint
    expect_equal(marker3$markers[[1]]$type, lint3[[1]]$type),
    expect_equal(marker3$markers[[1]]$file, file.path("test", lint3[[1]]$filename)),
    expect_equal(marker3$markers[[1]]$line, lint3[[1]]$line_number),
    expect_equal(marker3$markers[[1]]$column, lint3[[1]]$column_number),
    expect_equal(marker3$markers[[1]]$message, lint3[[1]]$message)
  )
})
test_that("it returns an empty list of markers if there are no lints", {
  with_mock(`rstudioapi::callFun` = function(...) return(list(...)),
    lint4 <- structure(
      list(),
      class = "lints"
    ),
    marker4 <- rstudio_source_markers(lint4),

    expect_equal(marker4$name, "lintr"),
    expect_equal(marker4$markers, list())
  )
})
