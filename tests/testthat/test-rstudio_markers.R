test_that("it returns markers which match lints", {
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
  with_mock(
    `rstudioapi::callFun` = function(...) list(...),
    `rstudioapi::executeCommand` = function(...) NULL,
    marker1 <- rstudio_source_markers(lint1)
  )
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
  with_mock(
    `rstudioapi::callFun` = function(...) list(...),
    `rstudioapi::executeCommand` = function(...) NULL,
    marker2 <- rstudio_source_markers(lint2)
  )
  expect_equal(marker2$name, "lintr")
  expect_equal(marker2$markers[[1]]$type, lint2[[1]]$type)
  expect_equal(marker2$markers[[1]]$file, lint2[[1]]$filename)
  expect_equal(marker2$markers[[1]]$line, lint2[[1]]$line_number)
  expect_equal(marker2$markers[[1]]$column, lint2[[1]]$column_number)
  expect_equal(marker2$markers[[1]]$message, lint2[[1]]$message)
})

test_that("it prepends the package path if it exists", {
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
  with_mock(
    `rstudioapi::callFun` = function(...) list(...),
    `rstudioapi::executeCommand` = function(...) NULL,
    marker3 <- rstudio_source_markers(lint3)
  )
  expect_equal(marker3$name, "lintr")
  expect_equal(marker3$basePath, "test")
  expect_equal(marker3$markers[[1]]$type, lint3[[1]]$type)
  expect_equal(marker3$markers[[1]]$file, file.path("test", lint3[[1]]$filename))
  expect_equal(marker3$markers[[1]]$line, lint3[[1]]$line_number)
  expect_equal(marker3$markers[[1]]$column, lint3[[1]]$column_number)
  expect_equal(marker3$markers[[1]]$message, lint3[[1]]$message)
})

test_that("it returns an empty list of markers if there are no lints", {
  lint4 <- structure(
    list(),
    class = "lints"
  )
  with_mock(
    `rstudioapi::callFun` = function(...) list(...),
    `rstudioapi::executeCommand` = function(...) NULL,
    marker4 <- rstudio_source_markers(lint4)
  )
  expect_equal(marker4$name, "lintr")
  expect_equal(marker4$markers, list())
})

test_that("rstudio_source_markers apply to print within rstudio", {
  with_mock(
    `rstudioapi::hasFun` = function(x, ...) TRUE,
    `rstudioapi::callFun` = function(...) cat("matched\n"), {
      writeLines("1:ncol(x)", tmp <- tempfile())
      on.exit(unlink(tmp))

      old <- options(lintr.rstudio_source_markers = TRUE)
      on.exit(options(old), add = TRUE)

      l <- lint(tmp, seq_linter())

      expect_output(print(l), "matched", fixed = TRUE)

      empty <- tempfile()
      file.create(empty)
      on.exit(unlink(empty), add = TRUE)

      l <- lint(empty, seq_linter())

      expect_output(print(l), "matched", fixed = TRUE)
    }
  )
})
