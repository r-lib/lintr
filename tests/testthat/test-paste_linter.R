test_that("paste_linter skips allowed usages for sep=''", {
  linter <- paste_linter()

  expect_lint("paste('a', 'b', 'c')", NULL, linter)
  expect_lint("paste('a', 'b', 'c', sep = ',')", NULL, linter)
  expect_lint("paste('a', 'b', collapse = '')", NULL, linter)
  expect_lint("cat(paste('a', 'b'), sep = '')", NULL, linter)
  expect_lint("sep <- ''; paste('a', sep)", NULL, linter)
  expect_lint("paste(sep = ',', '', 'a')", NULL, linter)
  expect_lint("paste0('a', 'b', 'c')", NULL, linter)
})

test_that("paste_linter blocks simple disallowed usages for sep=''", {
  expect_lint(
    "paste(sep = '', 'a', 'b')",
    rex::rex('paste0(...) is better than paste(..., sep = "").'),
    paste_linter()
  )

  expect_lint(
    "paste('a', 'b', sep = '')",
    rex::rex('paste0(...) is better than paste(..., sep = "").'),
    paste_linter()
  )
})

test_that("paste_linter skips allowed usages for collapse=', '", {
  linter <- paste_linter()

  expect_lint("paste('a', 'b', 'c')", NULL, linter)
  expect_lint("paste(x, sep = ', ')", NULL, linter)
  expect_lint("paste(x, collapse = ',')", NULL, linter)
  expect_lint("paste(foo(x), collapse = '/')", NULL, linter)
  # harder to catch statically
  expect_lint("collapse <- ', '; paste(x, collapse = collapse)", NULL, linter)

  # paste(..., sep=sep, collapse=", ") is not a trivial swap to toString
  expect_lint("paste(x, y, sep = '.', collapse = ', ')", NULL, linter)
  # any call involving ...length() > 1 will implicitly use the default sep
  expect_lint("paste(x, y, collapse = ', ')", NULL, linter)
  expect_lint("paste0(x, y, collapse = ', ')", NULL, linter)

  expect_lint("toString(x)", NULL, linter)

  # string match of ", " is OK -- lint only _exact_ match
  expect_lint('paste(x, collapse = ", \n")', NULL, linter)
})

test_that("paste_linter blocks simple disallowed usages for collapse=', '", {
  expect_lint(
    "paste(collapse = ', ', x)",
    rex::rex('toString(.) is more expressive than paste(., collapse = ", ")'),
    paste_linter()
  )

  expect_lint(
    "paste0(foo(x), collapse = ', ')",
    rex::rex('toString(.) is more expressive than paste(., collapse = ", ")'),
    paste_linter()
  )
})

test_that("paste_linter respects non-default arguments", {
  expect_lint("paste(sep = '', 'a', 'b')", NULL, paste_linter(allow_empty_sep = TRUE))
  expect_lint("paste('a', 'b', sep = '')", NULL, paste_linter(allow_empty_sep = TRUE))

  expect_lint("paste(collapse = ', ', x)", NULL, paste_linter(allow_to_string = TRUE))
  expect_lint("paste0(foo(x), collapse = ', ')", NULL, paste_linter(allow_to_string = TRUE))
})

test_that("paste_linter works for raw strings", {
  skip_if_not_r_version("4.0.0")
  expect_lint("paste(a, b, sep = R'(xyz)')", NULL, paste_linter())
  expect_lint(
    'paste(a, b, sep = R"---[]---")',
    rex::rex('paste0(...) is better than paste(..., sep = "").'),
    paste_linter()
  )

  expect_lint("paste(x, collapse = R'(,,)')", NULL, paste_linter())
  expect_lint(
    'paste(c(a, b, c), collapse = R"-{, }-")',
    rex::rex('toString(.) is more expressive than paste(., collapse = ", ")'),
    paste_linter()
  )
})

test_that("paste_linter catches use of paste0 with sep=", {
  expect_lint(
    "paste0(x, y, sep = '')",
    rex::rex("sep= is not a formal argument to paste0();"),
    paste_linter()
  )
})

test_that("paste_linter skips allowed usages for strrep()", {
  linter <- paste_linter()

  expect_lint("paste(x, collapse = '')", NULL, linter)
  expect_lint("paste(rep('*', 10), collapse = '+')", NULL, linter)
  expect_lint("paste(rep(c('a', 'b'), 2), collapse = '')", NULL, linter)
  expect_lint("paste0(rep('a', 2), 'b', collapse = '')", NULL, linter)
  # no collapse
  expect_lint("paste(rep('*', 10))", NULL, linter)
  # combined before aggregating
  expect_lint("paste(rep('*', 10), rep('x', 10), collapse = '')", NULL, linter)
})

test_that("paste_linter blocks simple disallowed usages", {
  linter <- paste_linter()
  lint_msg <- rex::rex("strrep(x, times) is better than paste")

  expect_lint("paste0(rep('*', 20L), collapse='')", lint_msg, linter)
  expect_lint("paste(rep('#', width), collapse='')", lint_msg, linter)
})

test_that("paste_linter skips allowed usages for file paths", {
  linter <- paste_linter()

  expect_lint("paste('a', 'b', 'c')", NULL, linter)
  expect_lint("paste('a', 'b', 'c', sep = ',')", NULL, linter)
  expect_lint("paste('a', 'b', collapse = '/')", NULL, linter)
  expect_lint("cat(paste('a', 'b'), sep = '/')", NULL, linter)
  expect_lint("sep <- '/'; paste('a', sep)", NULL, linter)
  expect_lint("paste(sep = ',', '/', 'a')", NULL, linter)

  # paste(..., sep='/', collapse=collapse) is not a trivial swap to file.path
  expect_lint("paste(x, y, sep = '/', collapse = ':')", NULL, linter)

  expect_lint("file.path('a', 'b', 'c')", NULL, linter)

  # testing the sep starts with / is not enough
  expect_lint("paste('a', 'b', sep = '//')", NULL, linter)
})

test_that("paste_linter blocks simple disallowed usages for file paths", {
  linter <- paste_linter()
  lint_msg <- rex::rex("Construct file paths with file.path(...) instead of")

  expect_lint("paste(sep = '/', 'a', 'b')", lint_msg, linter)
  expect_lint("paste('a', 'b', sep = '/')", lint_msg, linter)
})

test_that("paste_linter ignores non-path cases with paste0", {
  linter <- paste_linter()

  expect_lint("paste0(x, y)", NULL, linter)
  expect_lint("paste0('abc', 'def')", NULL, linter)
  expect_lint("paste0('/abc', 'def/')", NULL, linter)
  expect_lint("paste0(x, 'def/')", NULL, linter)
  expect_lint("paste0('/abc', y)", NULL, linter)
  expect_lint("paste0(foo(x), y)", NULL, linter)
  expect_lint("paste0(foo(x), 'def')", NULL, linter)

  # these might be a different lint (as.character instead, e.g.) but not here
  expect_lint("paste0(x)", NULL, linter)
  expect_lint("paste0('a')", NULL, linter)
  expect_lint("paste0('a', 1)", NULL, linter)

  # paste0(..., collapse=collapse) not directly mapped to file.path
  expect_lint("paste0(x, collapse = '/')", NULL, linter)
})

test_that("paste_linter detects paths built with '/' and paste0", {
  linter <- paste_linter()
  lint_msg <- rex::rex("Construct file paths with file.path(...) instead of")

  expect_lint("paste0(x, '/', y)", lint_msg, linter)
  expect_lint("paste0(x, '/', y, '/', z)", lint_msg, linter)
  expect_lint("paste0(x, '/abc/', 'def/', y)", lint_msg, linter)
  expect_lint("paste0(foo(x), '/abc/', 'def/', bar(y))", lint_msg, linter)
})

test_that("paste_linter skips initial/terminal '/' and repeated '/' for paths", {
  linter <- paste_linter()

  expect_lint("paste0('/', x)", NULL, linter)
  expect_lint("paste0(x, '/')", NULL, linter)
  expect_lint("paste0(x, '//hey/', y)", NULL, linter)
  expect_lint("paste0(x, '/hey//', y)", NULL, linter)
})

test_that("paste_linter doesn't skip all initial/terminal '/' for paths", {
  linter <- paste_linter()
  lint_msg <- rex::rex("Construct file paths with file.path(...) instead of")

  expect_lint('paste0("/abc/", "def")', lint_msg, linter)
  expect_lint('paste0("abc/", "def/")', lint_msg, linter)
})

test_that("multiple path lints are generated correctly", {
  expect_lint(
    trim_some("{
      paste(x, y, sep = '/')
      paste0(x, '/', y)
    }"),
    list(
      rex::rex('paste(..., sep = "/")'),
      rex::rex('paste0(x, "/", y, "/", z)')
    ),
    paste_linter()
  )
})

test_that("allow_file_path argument works", {
  expect_lint("paste(x, y, sep = '/')", NULL, paste_linter(allow_file_path = TRUE))
})