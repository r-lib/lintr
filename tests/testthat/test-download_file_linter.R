test_that("download_file_linter skips allowed usages", {
  linter <- download_file_linter()

  expect_no_lint("download.file(x, mode = 'wb')", linter)
  expect_no_lint("download.file(x, mode = 'ab')", linter)

  # mode specified by position
  expect_no_lint("download.file(x, , , , 'wb')", linter)
  expect_no_lint("download.file(x, , , , 'ab')", linter)

  # 'w' or 'a' but passed to different arguments
  expect_no_lint("download.file(x, destfile = 'w', mode = 'wb')", linter)
  expect_no_lint("download.file(x, mode = 'wb', method = 'internal', quiet = TRUE, 'w')", linter)
})

test_that("download_file_linter blocks simple disallowed usages", {
  linter <- download_file_linter()
  lint_message <- rex::rex("download.file() should use mode = 'wb' (or 'ab')")

  # Case 1: implicit default (mode = "w")
  expect_lint("download.file(x)", lint_message, linter)

  # Case 2: non-portable mode specified by name
  expect_lint("download.file(x, mode = 'w')", lint_message, linter)
  expect_lint("download.file(x, mode = 'a')", lint_message, linter)

  # Case 3: non-portable mode specified by position
  expect_lint("download.file(x, , , , mode = 'w')", lint_message, linter)

  # 'wb' passed to different argument
  expect_lint("download.file(x, mode = 'w', method = 'internal', quiet = TRUE, 'wb')", lint_message, linter)
  expect_lint("download.file(cacheOK = TRUE, destfile, method, quiet, x = 'wb')", lint_message, linter)
})

test_that("lints vectorize", {
  lint_message <- rex::rex("download.file() should use mode = 'wb' (or 'ab')")

  expect_lint(
    trim_some("{
      download.file(x, mode = 'w')
      download.file(y, mode = 'a')
    }"),
    list(
      list(lint_message, line_number = 2L),
      list(lint_message, line_number = 3L)
    ),
    download_file_linter()
  )
})
