context("absolute_path_linter")

test_that("unquote", {
  f <- lintr:::unquote
  expect_equal(f(character()), character())
  expect_equal(f("foo"), "foo")
  expect_equal(
    f(c("'f", "\"f'", "\"f\""), q="\""),
      c("'f", "\"f'",   "f"  ))
  expect_equal(
    f(c("\"f\"", "'f'", "`f`", "`'f'`"), q="'"),
      c("\"f\"",  "f",  "`f`", "`'f'`"))
  expect_equal(f("`a\\`b`", q=c("`")), "a`b")
  x <- c("\"x\"", "\"\\n\"", "\"\\\\\"", "\"\\\\y\"", "\"\\ny\"", "\"\\\\ny\"", "\"\\\\\\ny\"",
         "\"\\\\\\\\ny\"", "\"'\"", "\"\\\"\"", "\"`\"")
  y <- c("x", "\n", "\\", "\\y", "\ny", "\\ny", "\\\ny", "\\\\ny", "'", "\"", "`")
  expect_equal(f(x, q="\""), y)
})

test_that("unescape", {
  f <- lintr:::unescape
  expect_equal(f(character()), character())
  expect_equal(f("n"), "n")
  x <- c("x", "x\\n", "x\\\\", "x\\\\y", "x\\ny", "x\\\\ny",  "x\\\\\\ny", "x\\\\\\\\ny")
  y <- c("x", "x\n",  "x\\",   "x\\y",   "x\ny",  "x\\ny",    "x\\\ny",    "x\\\\ny")
  expect_equal(f(x), y)
})

test_that("is_root_path", {
  f <- lintr:::is_root_path

  x <- character()
  y <- logical()
  expect_equal(f(x), y)

  x <- c(   "", "foo", "http://rseek.org/",  "./",  " /", "/foo", "'/'")
  y <- c(FALSE, FALSE,               FALSE, FALSE, FALSE,  FALSE, FALSE)
  expect_equal(f(x), y)

  x <- c( "/",  "//")
  y <- c(TRUE, FALSE)
  expect_equal(f(x), y)

  x <- c( "~", "~/", "~//", "~bob2", "~foo_bar/")
  y <- c(TRUE, TRUE,  TRUE,    TRUE,        TRUE)
  expect_equal(f(x), y)

  x <- c("c:", "C:\\", "D:/", "C:\\\\", "D://")
  y <- c(TRUE,  TRUE,   TRUE,    FALSE,  FALSE)
  expect_equal(f(x), y)

  x <- c("\\\\", "\\\\localhost", "\\\\localhost\\")
  y <- c(  TRUE,            TRUE,              TRUE)
  expect_equal(f(x), y)
})


test_that("is_absolute_path", {
  f <- lintr:::is_absolute_path

  x <- character()
  y <- logical()
  expect_equal(f(x), y)

  x <- c( "/",  "//", "/foo", "/foo/")
  y <- c(TRUE, FALSE,   TRUE,    TRUE)
  expect_equal(f(x), y)

  x <- c( "~", "~/foo", "~/foo/",  "~'")  #nolint
  y <- c(TRUE,    TRUE,     TRUE, FALSE)
  expect_equal(f(x), y)

  x <- c("c:", "C:\\foo\\", "C:/foo/")  #nolint
  y <- c(TRUE,        TRUE,      TRUE)
  expect_equal(f(x), y)

  x <- c("\\\\", "\\\\localhost", "\\\\localhost\\c$", "\\\\localhost\\c$\\foo")  #nolint
  y <- c(  TRUE,            TRUE,                TRUE,                     TRUE)
  expect_equal(f(x), y)
})


test_that("is_relative_path", {
  f <- lintr:::is_relative_path

  x <- character()
  y <- logical()
  expect_equal(f(x), y)

  x <- c(  "/", "c:\\",  "~/", "foo", "http://rseek.org/", "'./'")
  y <- c(FALSE,  FALSE, FALSE, FALSE,               FALSE,  FALSE)
  expect_equal(f(x), y)

  x <- c("/foo", "foo/", "foo/bar", "foo//bar", "./foo", "../foo")  #nolint
  y <- c( FALSE,   TRUE,      TRUE,       TRUE,    TRUE,     TRUE)
  expect_equal(f(x), y)

  x <- c("\\\\", "\\foo", "foo\\", "foo\\bar", ".\\foo", "..\\foo",  ".", "..", "../")  #nolint
  y <- c( FALSE,   FALSE,    TRUE,       TRUE,     TRUE,      TRUE, TRUE, TRUE,  TRUE)
  expect_equal(f(x), y)
})


test_that("is_path", {
  f <- lintr:::is_path

  x <- character()
  y <- logical()
  expect_equal(f(x), y)

  x <- c(   "",  "foo", "http://rseek.org/", "foo\nbar", "'foo/bar'", "'/'")
  y <- c(FALSE,  FALSE,               FALSE,      FALSE,       FALSE, FALSE)
  expect_equal(f(x), y)

  x <- c("c:", "..", "foo/bar", "foo\\bar",  "~", "\\\\localhost")  #nolint
  y <- c(TRUE, TRUE,      TRUE,       TRUE, TRUE,            TRUE)
  expect_equal(f(x), y)
})


test_that("is_valid_path", {
  f <- lintr:::is_valid_path

  x <- character()
  y <- logical()
  expect_equal(f(x), y)

  x <- c("C:/asdf", "C:/asd*f", "a\\s:df", "a\\\nsdf")  #nolint
  y <- c(     TRUE,      FALSE,     FALSE,      FALSE)
  expect_equal(f(x), y)

  x <- c("C:/asdf", "C:/asd*f", "a\\s:df", "a\\\nsdf")  #nolint
  y <- c(     TRUE,      FALSE,     FALSE,      FALSE)
  expect_equal(f(x, lax=TRUE), y)

  x <- c("/asdf", "/asd*f", "/as:df", "/a\nsdf")
  y <- c(   TRUE,    TRUE,    TRUE,        TRUE)
  expect_equal(f(x), y)

  x <- c("/asdf", "/asd*f", "/as:df", "/a\nsdf")
  y <- c(   TRUE,    FALSE,    FALSE,     FALSE)
  expect_equal(f(x, lax=TRUE), y)
})


test_that("is_long_path", {
  f <- lintr:::is_long_path

  x <- character()
  y <- logical()
  expect_equal(f(x), y)

  x <- c("foo/", "/foo", "n/a", "Z:\\foo", "foo/bar", "~/foo", "../foo")  #nolint
  y <- c( FALSE,  FALSE, FALSE,      TRUE,      TRUE,    TRUE,     TRUE)
  expect_equal(f(x), y)
})


test_that("returns the correct linting", {
  msg <- rex::escape("Do not use absolute paths.")

  # strict mode
  linter <- absolute_path_linter(lax=FALSE)
  expect_lint("'..'", NULL, linter)
  expect_lint("'./blah'", NULL, linter)
  expect_lint(encodeString("'blah\\file.txt'"), NULL, linter)
  expect_lint("\"'/'\"", NULL, linter)

  expect_lint("'/'", msg, linter)
  expect_lint("'/blah/file.txt'", msg, linter)
  expect_lint(encodeString("'d:\\'"), msg, linter)
  expect_lint("'E:/blah/file.txt'", msg, linter)
  expect_lint(encodeString("'\\\\'"), msg, linter)
  expect_lint(encodeString("'\\\\server\\path'"), msg, linter)
  expect_lint("'~'", msg, linter)
  expect_lint("'~james.hester/blah/file.txt'", msg, linter)
  expect_lint(encodeString("'/a\nsdf'"), msg, linter)
  expect_lint("'/as:df'", msg, linter)

  # lax mode: no check for strings that are likely not paths (too short or with special characters)
  linter <- absolute_path_linter(lax=TRUE)
  expect_lint("'/'", NULL, linter)
  expect_lint(encodeString("'/a\nsdf/bar'"), NULL, linter)
  expect_lint("'/as:df/bar'", NULL, linter)
})
