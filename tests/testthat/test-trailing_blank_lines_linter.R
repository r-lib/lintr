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
  tmp <- withr::local_tempfile()
  tmp2 <- withr::local_tempfile()
  cat("lm(y ~ x)\n", file = tmp)
  cat("lm(y ~ x)", file = tmp2)

  expect_lint(content = NULL, file = tmp, NULL, linter)
  expect_lint(content = NULL, file = tmp2, list(
    message = msg2,
    line_number = 1L,
    column_number = 10L
  ), linter)

  # Construct an Rmd file without terminal newline
  tmp3 <- withr::local_tempfile(fileext = ".Rmd")
  cat(
    trim_some(
      '---
      title: "Some file"
      ---

      ```{r}
      abc = 123
      ```

      ```{r child="some-file.Rmd"}
      ```'
    ),
    file = tmp3
  )
  expect_lint(content = NULL, file = tmp3, list(
    message = msg2,
    line_number = 10L,
    # We can't get 4 here because the line is NA-masked in get_source_expressions(), so no line length info exists.
    column_number = 1L
  ), linter)

  # Construct an Rmd file without R code (#1415)
  tmp4 <- withr::local_tempfile(fileext = ".Rmd")
  cat(
    trim_some(
      '---
      title: "Some file"
      ---

      No code and no terminal newline'
    ),
    file = tmp4
  )
  expect_lint(content = NULL, file = tmp4, list(
    message = msg2,
    line_number = 5L,
    # We can't get 4 here because the line is NA-masked in get_source_expressions(), so no line length info exists.
    column_number = 1L
  ), linter)
})
