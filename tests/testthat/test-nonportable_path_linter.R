context("nonportable_path_linter")


test_that("Non-portable path linter", {
  linter <- nonportable_path_linter(lax=FALSE)
  msg <- rex::escape("Use file.path() to construct portable file paths.")

  # various strings
  expect_lint("'foo'", NULL, linter)
  expect_lint("'https://cran.r-project.org/web/packages/lintr/'", NULL, linter)
  expect_lint(encodeString("'hello\nthere!'"), NULL, linter)
  expect_lint("\"'/foo'\"", NULL, linter)

  # system root
  expect_lint("'/'", NULL, linter)
  expect_lint("'~'", NULL, linter)
  expect_lint("'c:'", NULL, linter)
  expect_lint("'.'", NULL, linter)

  # paths with (back)slashes
  expect_lint("'~/'", msg, linter)
  expect_lint("'c:/'", msg, linter)
  expect_lint(encodeString("'D:\\'"), msg, linter)
  expect_lint("'../'", msg, linter)
  expect_lint("'/foo'", msg, linter)
  expect_lint("'foo/'", msg, linter)
  expect_lint("'foo/bar'", msg, linter)
  expect_lint(encodeString("'foo\\bar'"), msg, linter)

  expect_lint("'/as:df'", msg, linter)
  expect_lint(encodeString("'/a\nsdf'"), msg, linter)

  # lax mode: no check for strings that are likely not paths (too short or with special characters)
  linter <- nonportable_path_linter(lax=TRUE)

  expect_lint("'/foo'", NULL, linter)
  expect_lint(encodeString("'/a\nsdf/bar'"), NULL, linter)
  expect_lint("'/as:df/bar'", NULL, linter)
})
