test_that("trailing_blank_lines_linter doesn't block allowed usages", {
  linter <- trailing_blank_lines_linter()

  expect_lint("blah", NULL, linter)
  expect_lint("blah <- 1  ", NULL, linter)
  expect_lint("blah <- 1\nblah", NULL, linter)
  expect_lint("blah <- 1\nblah\n \n blah", NULL, linter)

  tmp <- withr::local_tempfile(lines = "lm(y ~ x)")
  expect_lint(file = tmp, checks = NULL, linters = linter)
})

test_that("trailing_blank_lines_linter detects disallowed usages", {
  linter <- trailing_blank_lines_linter()
  lint_msg <- rex::rex("Trailing blank lines are superfluous.")

  expect_lint("blah <- 1\n", lint_msg, linter)
  expect_lint("blah <- 1\n  ", lint_msg, linter)
  expect_lint("blah <- 1\n \n ", list(lint_msg, lint_msg), linter)
  expect_lint("blah <- 1\n\n", list(lint_msg, lint_msg), linter)
  expect_lint("blah <- 1\n\t\n", list(lint_msg, lint_msg), linter)

  # Construct a test file without terminal newline
  # cf. test-get_source_expressions.R
  tmp2 <- withr::local_tempfile()
  cat("lm(y ~ x)", file = tmp2)
  expect_lint(
    file = tmp2,
    checks = list(
      message = rex::rex("Missing terminal newline."),
      line_number = 1L,
      column_number = 10L
    ),
    linters = linter
  )
})

test_that("trailing_blank_lines_linter detects missing terminal newlines in Rmd/qmd docs", {
  linter <- trailing_blank_lines_linter()

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
  expect_lint(
    file = tmp3,
    checks = list(
      message = rex::rex("Missing terminal newline."),
      line_number = 10L,
      # We can't get 4 here because the line is NA-masked in get_source_expressions(), so no line length info exists.
      column_number = 1L
    ),
    linters = linter
  )

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
  expect_lint(
    file = tmp4,
    checks = list(
      message = rex::rex("Missing terminal newline."),
      line_number = 5L,
      # We can't get 4 here because the line is NA-masked in get_source_expressions(), so no line length info exists.
      column_number = 1L
    ),
    linters = linter
  )

  # Construct a qmd file without terminal newline
  tmp5 <- withr::local_tempfile(fileext = ".qmd")
  cat(
    trim_some(
      '---
      title: "Some file"
      ---

      ```{r}
      abc = 123
      ```

      ```{r child="some-file.qmd"}
      ```'
    ),
    file = tmp5
  )
  expect_lint(
    file = tmp5,
    checks = list(
      message = rex::rex("Missing terminal newline."),
      line_number = 10L,
      # We can't get 4 here because the line is NA-masked in get_source_expressions(), so no line length info exists.
      column_number = 1L
    ),
    linters = linter
  )
})

test_that("blank lines in knitr chunks produce lints", {
  linter <- trailing_blank_lines_linter()

  tmp6 <- withr::local_tempfile(
    fileext = ".Rmd",
    lines = trim_some(
      '---
      title: "Some file"
      ---

      ```{r}
      abc = 123

      ```
      \n'
    )
  )

  expect_lint(
    file = tmp6,
    checks = list(message = rex::rex("Trailing blank lines are superfluous."), line_number = 7L, column_number = 1L),
    linters = linter
  )

  tmp7 <- withr::local_tempfile(
    fileext = ".qmd",
    lines = trim_some(
      '---
      title: "Some file"
      ---

      ```{r}
      abc = 123



      ```
      \n'
    )
  )

  expect_lint(
    file = tmp7,
    checks = list(
      list(message = rex::rex("Trailing blank lines are superfluous."), line_number = 7L, column_number = 1L),
      list(message = rex::rex("Trailing blank lines are superfluous."), line_number = 8L, column_number = 1L),
      list(message = rex::rex("Trailing blank lines are superfluous."), line_number = 9L, column_number = 1L)
    ),
    linters = linter
  )
})
