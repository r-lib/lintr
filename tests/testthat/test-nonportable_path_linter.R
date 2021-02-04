test_that("Non-portable path linter", {
  linter <- nonportable_path_linter(lax = FALSE)
  msg <- rex::escape("Use file.path() to construct portable file paths.")

  # various strings
  non_path_strings <- c(
    "foo",
    "https://cran.r-project.org/web/packages/lintr/",
    encodeString("hello\nthere!")
  )
  for (path in non_path_strings) {
    expect_lint(single_quote(path), NULL, linter)
    expect_lint(double_quote(path), NULL, linter)
  }

  expect_lint("\"'/foo'\"", NULL, linter) # nested quotes

  # system root
  root_path_strings <- c("/", "~", "c:", ".")
  for (path in root_path_strings) {
    expect_lint(single_quote(path), NULL, linter)
    expect_lint(double_quote(path), NULL, linter)
  }

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
    expect_lint(single_quote(path), msg, linter)
    expect_lint(double_quote(path), msg, linter)
  }

  # lax mode: no check for strings that are likely not paths (too short or with special characters)
  linter <- nonportable_path_linter(lax = TRUE)

  unlikely_path_strings <- c(
    "/foo", encodeString("/a\nsdf/bar"), "/as:df/bar"
  )
  for (path in unlikely_path_strings) {
    expect_lint(single_quote(path), NULL, linter)
    expect_lint(double_quote(path), NULL, linter)
  }
})
