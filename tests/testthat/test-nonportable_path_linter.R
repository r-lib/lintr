test_that("nonportable_path_linter skips allowed usages", {
  linter <- nonportable_path_linter(lax = FALSE)

  # various strings
  non_path_strings <- c(
    "foo",
    "https://cran.r-project.org/web/packages/lintr/",
    encodeString("hello\nthere!")
  )
  for (path in non_path_strings) {
    expect_no_lint(single_quote(path), linter)
    expect_no_lint(double_quote(path), linter)
  }

  expect_no_lint("\"'/foo'\"", linter) # nested quotes

  # system root
  root_path_strings <- c("/", "~", "c:", ".")
  for (path in root_path_strings) {
    expect_no_lint(single_quote(path), linter)
    expect_no_lint(double_quote(path), linter)
  }
})

test_that("nonportable_path_linter blocks disallowed usages", {
  linter <- nonportable_path_linter(lax = FALSE)
  lint_msg <- rex::escape("Use file.path() to construct portable file paths.")

  # paths with (back)slashes
  slash_path_strings <- c(
    "~/",
    "c:/",
    encodeString("D:\\"),
    "../",
    "/foo",
    "foo/",
    "foo/bar",
    encodeString("foo\\bar"),
    "/as:df",
    encodeString("/a\nsdf")
  )
  for (path in slash_path_strings) {
    expect_lint(single_quote(path), lint_msg, linter)
    expect_lint(double_quote(path), lint_msg, linter)
  }
})

test_that("nonportable_path_linter's lax argument works", {
  # lax mode: no check for strings that are likely not paths (too short or with special characters)
  linter <- nonportable_path_linter(lax = TRUE)

  unlikely_path_strings <- c(
    "/foo", encodeString("/a\nsdf/bar"), "/as:df/bar"
  )
  for (path in unlikely_path_strings) {
    expect_no_lint(single_quote(path), linter)
    expect_no_lint(double_quote(path), linter)
  }
})

test_that("lints vectorize", {
  lint_msg <- rex::escape("Use file.path() to construct portable file paths.")

  expect_lint(
    trim_some("{
      '~/'
      'C:/'
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    nonportable_path_linter(lax = FALSE)
  )
})
