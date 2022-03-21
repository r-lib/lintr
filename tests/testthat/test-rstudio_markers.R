test_that("it returns markers which match lints", {
  mockery::stub(rstudio_source_markers, "rstudioapi::callFun", function(...) list(...))
  mockery::stub(rstudio_source_markers, "rstudioapi::executeCommand", function(...) NULL)

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
  )

  marker1 <- rstudio_source_markers(lint1)
  expect_equal(marker1$name, "lintr")
  expect_equal(marker1$markers[[1]]$type, lint1[[1]]$type)
  expect_equal(marker1$markers[[1]]$file, lint1[[1]]$filename)
  expect_equal(marker1$markers[[1]]$line, lint1[[1]]$line_number)
  expect_equal(marker1$markers[[1]]$column, lint1[[1]]$column_number)
  expect_equal(marker1$markers[[1]]$message, lint1[[1]]$message)

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
  )
  marker2 <- rstudio_source_markers(lint2)
  expect_equal(marker2$name, "lintr")
  expect_equal(marker2$markers[[1]]$type, lint2[[1]]$type)
  expect_equal(marker2$markers[[1]]$file, lint2[[1]]$filename)
  expect_equal(marker2$markers[[1]]$line, lint2[[1]]$line_number)
  expect_equal(marker2$markers[[1]]$column, lint2[[1]]$column_number)
  expect_equal(marker2$markers[[1]]$message, lint2[[1]]$message)
})

test_that("it prepends the package path if it exists", {
  mockery::stub(rstudio_source_markers, "rstudioapi::callFun", function(...) list(...))
  mockery::stub(rstudio_source_markers, "rstudioapi::executeCommand", function(...) NULL)

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
  )
  marker3 <- rstudio_source_markers(lint3)
  expect_equal(marker3$name, "lintr")
  expect_equal(marker3$basePath, "test")
  expect_equal(marker3$markers[[1]]$type, lint3[[1]]$type)
  expect_equal(marker3$markers[[1]]$file, file.path("test", lint3[[1]]$filename))
  expect_equal(marker3$markers[[1]]$line, lint3[[1]]$line_number)
  expect_equal(marker3$markers[[1]]$column, lint3[[1]]$column_number)
  expect_equal(marker3$markers[[1]]$message, lint3[[1]]$message)
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
  # TODO(@michaelchirico): Recent (as of this writing) withr v2.5.0 supports local_tempfile(lines = l) to simplify this
  tmp <- withr::local_tempfile()
  writeLines("1:ncol(x)", tmp)
  empty <- withr::local_tempfile()
  file.create(empty)

  mockery::stub(print.lints, "rstudioapi::hasFun", function(x, ...) TRUE)
  mockery::stub(print.lints, "rstudio_source_markers", function(x) cat("matched\n"))

  l <- lint(tmp, seq_linter())
  expect_output(print(l), "matched", fixed = TRUE)

  l <- lint(empty, seq_linter())
  expect_output(print(l), "matched", fixed = TRUE)
})
