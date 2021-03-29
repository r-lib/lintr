test_that("returns the correct linting", {
  linter <- trailing_blank_lines_linter()
  msg <- rex("Trailing blank lines are superfluous.")
  msg2 <- rex("Missing terminal newline.")

  expect_lint("blah", NULL, linter)
  expect_lint("blah <- 1  ", NULL, linter)
  expect_lint("blah <- 1\nblah", NULL, linter)
  expect_lint("blah <- 1\nblah\n \n blah", NULL, linter)

  expect_lint("blah <- 1\n", msg, linter)
  expect_lint("blah <- 1\n  ", msg, linter)

  expect_lint("blah <- 1\n \n ", list(msg, msg), linter)
  expect_lint("blah <- 1\n\n", list(msg, msg), linter)
  expect_lint("blah <- 1\n\t\n", list(msg, msg), linter)

  # Construct a test file without terminal newline
  # cf. test-get_source_expressions.R
  writeLines("lm(y ~ x)", tmp <- tempfile())
  on.exit(unlink(tmp), add = TRUE)
  writeBin(
    # strip the last (two) element(s) (\r\n or \n)
    head(
      readBin(tmp, raw(), file.size(tmp)),
      if (.Platform$OS.type == "windows") -2L else -1L
    ),
    tmp2 <- tempfile()
  )
  on.exit(unlink(tmp2), add = TRUE)

  expect_lint(content = NULL, file = tmp, NULL, linter)
  expect_lint(content = NULL, file = tmp2, list(
    message = msg2,
    line_number = 1L,
    column_number = 10L
  ), linter)
})
