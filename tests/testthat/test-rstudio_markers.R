test_that("it returns markers which match lints", {
  mockery::stub(rstudio_source_markers, "rstudioapi::callFun", function(...) list(...))
  mockery::stub(rstudio_source_markers, "rstudioapi::executeCommand", function(...) NULL)

  lint1 <- structure(
    list(
      Lint(
        filename = "test_file",
        line_number = 1L,
        column_number = 2L,
        type = "error",
        line = "a line",
        message = "hi"
      )
    ),
    class = "lints"
  )
  lint1[[1L]]$linter <- "linter_name"

  marker1 <- rstudio_source_markers(lint1)
  expect_equal(marker1$name, "lintr")
  expect_equal(marker1$markers[[1L]]$type, lint1[[1L]]$type)
  expect_equal(marker1$markers[[1L]]$file, lint1[[1L]]$filename)
  expect_equal(marker1$markers[[1L]]$line, lint1[[1L]]$line_number)
  expect_equal(marker1$markers[[1L]]$column, lint1[[1L]]$column_number)
  expect_equal(marker1$markers[[1L]]$message, paste0("[", lint1[[1L]]$linter, "] ", lint1[[1L]]$message))

  lint2 <- structure(
    list(
      Lint(
        filename = "test_file",
        line_number = 1L,
        column_number = 2L,
        type = "error",
        line = "a line",
        message = "hi"
      ),
      Lint(
        filename = "test_file2",
        line_number = 10L,
        column_number = 5L,
        type = "warning",
        message = "test a message"
      )
    ),
    class = "lints"
  )
  lint2[[1L]]$linter <- "linter_name"
  lint2[[2L]]$linter <- "linter_name"
  marker2 <- rstudio_source_markers(lint2)
  expect_equal(marker2$name, "lintr")
  expect_equal(marker2$markers[[1L]]$type, lint2[[1L]]$type)
  expect_equal(marker2$markers[[1L]]$file, lint2[[1L]]$filename)
  expect_equal(marker2$markers[[1L]]$line, lint2[[1L]]$line_number)
  expect_equal(marker2$markers[[1L]]$column, lint2[[1L]]$column_number)
  expect_equal(marker2$markers[[1L]]$message, paste0("[", lint2[[1L]]$linter, "] ", lint2[[1L]]$message))
})

test_that("it prepends the package path if it exists", {
  mockery::stub(rstudio_source_markers, "rstudioapi::callFun", function(...) list(...))
  mockery::stub(rstudio_source_markers, "rstudioapi::executeCommand", function(...) NULL)

  lint3 <- structure(
    list(
      Lint(
        filename = "test_file",
        line_number = 1L,
        column_number = 2L,
        type = "error",
        line = "a line",
        message = "hi"
      )
    ),
    class = "lints",
    path = "test"
  )
  lint3[[1L]]$linter <- "linter_name"
  marker3 <- rstudio_source_markers(lint3)
  expect_equal(marker3$name, "lintr")
  expect_equal(marker3$basePath, "test")
  expect_equal(marker3$markers[[1L]]$type, lint3[[1L]]$type)
  expect_equal(marker3$markers[[1L]]$file, file.path("test", lint3[[1L]]$filename))
  expect_equal(marker3$markers[[1L]]$line, lint3[[1L]]$line_number)
  expect_equal(marker3$markers[[1L]]$column, lint3[[1L]]$column_number)
  expect_equal(marker3$markers[[1L]]$message, paste0("[", lint3[[1L]]$linter, "] ", lint3[[1L]]$message))
})

test_that("it returns an empty list of markers if there are no lints", {
  mockery::stub(rstudio_source_markers, "rstudioapi::callFun", function(...) list(...))
  mockery::stub(rstudio_source_markers, "rstudioapi::executeCommand", function(...) NULL)

  lint4 <- structure(
    list(),
    class = "lints"
  )
  marker4 <- rstudio_source_markers(lint4)
  expect_equal(marker4$name, "lintr")
  expect_equal(marker4$markers, list())
})

test_that("rstudio_source_markers apply to print within rstudio", {
  withr::local_options(lintr.rstudio_source_markers = TRUE)

  tmp <- withr::local_tempfile(lines = "1:ncol(x)")
  empty <- withr::local_tempfile(lines = character(0L))

  mockery::stub(print.lints, "rstudioapi::hasFun", function(x, ...) TRUE)
  mockery::stub(print.lints, "rstudio_source_markers", function(x) cat("matched\n"))

  l <- lint(tmp, seq_linter())
  expect_output(print(l), "matched", fixed = TRUE)

  l <- lint(empty, seq_linter())
  expect_output(print(l), "matched", fixed = TRUE)
})
