test_that("returns the correct linting", {
  lint_msg <- rex::escape("Do not use absolute paths.")

  # strict mode
  linter <- absolute_path_linter(lax = FALSE)
  non_absolute_path_strings <- c(
    "..",
    "./blah",
    encodeString("blah\\file.txt")
  )
  for (path in non_absolute_path_strings) {
    expect_no_lint(single_quote(path), linter)
    expect_no_lint(double_quote(path), linter)
  }

  expect_no_lint("\"'/'\"", linter) # nested quotes

  absolute_path_strings <- c(
    "/",
    "/blah/file.txt",
    encodeString("d:\\"),
    "E:/blah/file.txt",
    encodeString("\\\\"),
    encodeString("\\\\server\\path"),
    "~",
    "~james.hester/blah/file.txt",
    encodeString("/a\nsdf"),
    "/as:df"
  )
  for (path in absolute_path_strings) {
    expect_lint(single_quote(path), lint_msg, linter)
    expect_lint(double_quote(path), lint_msg, linter)
  }

  # lax mode: no check for strings that are likely not paths (too short or with special characters)
  linter <- absolute_path_linter(lax = TRUE)
  unlikely_path_strings <- c(
    "/",
    encodeString("/a\nsdf/bar"),
    "/as:df/bar"
  )
  for (path in unlikely_path_strings) {
    expect_no_lint(single_quote(path), linter)
    expect_no_lint(double_quote(path), linter)
  }
})

test_that("raw strings are handled correctly", {
  expect_no_lint('R"(./blah)"', absolute_path_linter(lax = FALSE))
  expect_lint(
    "R'--[/blah/file.txt]--'",
    rex::rex("Do not use absolute paths."),
    absolute_path_linter(lax = FALSE)
  )
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Do not use absolute paths.")

  expect_lint(
    trim_some("{
      '/'
      '/blah/file.txt'
      'abcdefg'
      '~'
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L),
      list(lint_msg, line_number = 5L)
    ),
    absolute_path_linter(lax = FALSE)
  )
})
