test_that("line_length_linter skips allowed usages", {
  linter <- line_length_linter(80L)

  expect_lint("blah", NULL, linter)
  expect_lint(strrep("x", 80L), NULL, linter)
})

test_that("line_length_linter blocks disallowed usages", {
  linter <- line_length_linter(80L)
  lint_msg <- rex::rex("Lines should not be more than 80 characters. This line is 81 characters.")

  expect_lint(
    strrep("x", 81L),
    list(
      message = lint_msg,
      column_number = 81L
    ),
    linter
  )

  expect_lint(
    paste(rep(strrep("x", 81L), 2L), collapse = "\n"),
    list(
      list(
        message = lint_msg,
        line_number = 1L,
        column_number = 81L
      ),
      list(
        message = lint_msg,
        line_number = 2L,
        column_number = 81L
      )
    ),
    linter
  )

  linter <- line_length_linter(20L)
  lint_msg <- rex::rex("Lines should not be more than 20 characters. This line is 22 characters.")
  expect_lint(strrep("a", 20L), NULL, linter)
  expect_lint(
    strrep("a", 22L),
    list(
      message = lint_msg,
      column_number = 21L
    ),
    linter
  )

  # Don't duplicate lints
  expect_length(
    lint(
      "x <- 2 # ------------\n",
      linters = linter,
      parse_settings = FALSE
    ),
    1L
  )
})

test_that("Multiple lints give custom messages", {
  expect_lint(
    trim_some("{
      abcdefg
      hijklmnop
    }"),
    list(
      list("9 characters", line_number = 2L),
      list("11 characters", line_number = 3L)
    ),
    line_length_linter(5L)
  )
})

test_that("string bodies can be ignored", {
  linter <- line_length_linter(10L, ignore_string_bodies = TRUE)
  lint_msg <- rex::rex("Lines should not be more than 10 characters. This line is 15 characters.")

  expect_no_lint(
    trim_some("
      1234567890
      str <- '
      123456789012345
      '
    "),
    linter
  )

  expect_no_lint(
    trim_some("
      1234567890
      str <- '90
      123456789012345
      123456789'
    "),
    linter
  )

  expect_lint(
    trim_some("
      1234567890
      str <- '9012345
      1234567890
      123456789'
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      1234567890
      str <- '90
      1234567890
      12345678'; 2345
    "),
    lint_msg,
    linter
  )

  expect_lint(
    "'1'; '2'; '345'",
    lint_msg,
    linter
  )
})
