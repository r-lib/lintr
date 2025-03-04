test_that("it returns markers which match lints", {
  skip_if_not_installed("rstudioapi")
  local_mocked_bindings(
    callFun = function(...) list(...),
    executeCommand = function(...) NULL,
    .package = "rstudioapi"
  )

  lint1 <- list(Lint(
    filename = "test_file",
    line_number = 1L,
    column_number = 2L,
    type = "error",
    line = "a line",
    message = "hi"
  ))
  class(lint1) <- "lints"
  lint1[[1L]]$linter <- "linter_name"

  marker1 <- rstudio_source_markers(lint1)
  expect_identical(marker1$name, "lintr")
  expect_identical(marker1$markers[[1L]]$type, lint1[[1L]]$type)
  expect_identical(marker1$markers[[1L]]$file, lint1[[1L]]$filename)
  expect_identical(marker1$markers[[1L]]$line, lint1[[1L]]$line_number)
  expect_identical(marker1$markers[[1L]]$column, lint1[[1L]]$column_number)
  expect_identical(marker1$markers[[1L]]$message, paste0("[", lint1[[1L]]$linter, "] ", lint1[[1L]]$message))

  lint2 <- list(
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
      column_number = 1L,
      type = "warning",
      message = "test a message"
    )
  )
  class(lint2) <- "lints"
  lint2[[1L]]$linter <- "linter_name"
  lint2[[2L]]$linter <- "linter_name"
  marker2 <- rstudio_source_markers(lint2)
  expect_identical(marker2$name, "lintr")
  expect_identical(marker2$markers[[1L]]$type, lint2[[1L]]$type)
  expect_identical(marker2$markers[[1L]]$file, lint2[[1L]]$filename)
  expect_identical(marker2$markers[[1L]]$line, lint2[[1L]]$line_number)
  expect_identical(marker2$markers[[1L]]$column, lint2[[1L]]$column_number)
  expect_identical(marker2$markers[[1L]]$message, paste0("[", lint2[[1L]]$linter, "] ", lint2[[1L]]$message))
})

test_that("it prepends the package path if it exists", {
  skip_if_not_installed("rstudioapi")
  local_mocked_bindings(
    callFun = function(...) list(...),
    executeCommand = function(...) NULL,
    .package = "rstudioapi"
  )

  lint3 <- list(Lint(
    filename = "test_file",
    line_number = 1L,
    column_number = 2L,
    type = "error",
    line = "a line",
    message = "hi"
  ))
  class(lint3) <- "lints"
  attr(lint3, "path") <- "test"
  lint3[[1L]]$linter <- "linter_name"
  marker3 <- rstudio_source_markers(lint3)
  expect_identical(marker3$name, "lintr")
  expect_identical(marker3$basePath, "test")
  expect_identical(marker3$markers[[1L]]$type, lint3[[1L]]$type)
  expect_identical(marker3$markers[[1L]]$file, file.path("test", lint3[[1L]]$filename))
  expect_identical(marker3$markers[[1L]]$line, lint3[[1L]]$line_number)
  expect_identical(marker3$markers[[1L]]$column, lint3[[1L]]$column_number)
  expect_identical(marker3$markers[[1L]]$message, paste0("[", lint3[[1L]]$linter, "] ", lint3[[1L]]$message))
})

test_that("it returns an empty list of markers if there are no lints", {
  skip_if_not_installed("rstudioapi")
  local_mocked_bindings(
    callFun = function(...) list(...),
    executeCommand = function(...) NULL,
    .package = "rstudioapi"
  )

  lint4 <- `class<-`(list(), "lints")
  marker4 <- rstudio_source_markers(lint4)
  expect_identical(marker4$name, "lintr")
  expect_identical(marker4$markers, list())
})

test_that("rstudio_source_markers apply to print within rstudio", {
  withr::local_options(lintr.rstudio_source_markers = TRUE)

  tmp <- withr::local_tempfile(lines = "1:ncol(x)")
  empty <- withr::local_tempfile(lines = character(0L))

  skip_if_not_installed("rstudioapi")
  local_mocked_bindings(
    hasFun = function(...) TRUE,
    .package = "rstudioapi"
  )
  local_mocked_bindings(rstudio_source_markers = function(x) cat("matched\n"))

  l <- lint(tmp, seq_linter())
  expect_output(print(l), "matched", fixed = TRUE)

  l <- lint(empty, seq_linter())
  expect_message(expect_output(print(l), "matched", fixed = TRUE), "No lints found")
})
