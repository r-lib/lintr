test_that("paste_linter skips allowed usages for sep=''", {
  linter <- paste_linter()

  expect_no_lint("paste('a', 'b', 'c')", linter)
  expect_no_lint("paste('a', 'b', 'c', sep = ',')", linter)
  expect_no_lint("paste('a', 'b', collapse = '')", linter)
  expect_no_lint("cat(paste('a', 'b'), sep = '')", linter)
  expect_no_lint("sep <- ''; paste('a', sep)", linter)
  expect_no_lint("paste(sep = ',', '', 'a')", linter)
  expect_no_lint("paste0('a', 'b', 'c')", linter)
  expect_no_lint("expression(2); paste('a', 'b', 'c', sep = ',')", linter)
  expect_no_lint("paste('a', 'b', expression(2), sep = ',')", linter)
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

  expect_lint(
    "paste('a', 'b', expression(2), sep = '')",
    rex::rex('paste0(...) is better than paste(..., sep = "").'),
    paste_linter()
  )

  expect_lint(
    "c(expression(paste('a', 'b', sep = ',')))",
    rex::rex("inside expression(...), paste does not accept a 'sep' argument."),
    paste_linter()
  )
  expect_lint(
    "c(expression(paste('a', 'b', sep = '')))",
    rex::rex("inside expression(...), paste does not accept a 'sep' argument."),
    paste_linter()
  )
})

test_that("paste_linter skips allowed usages for collapse=', '", {
  linter <- paste_linter()

  expect_no_lint("paste('a', 'b', 'c')", linter)
  expect_no_lint("paste(x, sep = ', ')", linter)
  expect_no_lint("paste(x, collapse = ',')", linter)
  expect_no_lint("paste(foo(x), collapse = '/')", linter)
  # harder to catch statically
  expect_no_lint("collapse <- ', '; paste(x, collapse = collapse)", linter)

  # paste(..., sep=sep, collapse=", ") is not a trivial swap to toString
  expect_no_lint("paste(x, y, sep = '.', collapse = ', ')", linter)
  # any call involving ...length() > 1 will implicitly use the default sep
  expect_no_lint("paste(x, y, collapse = ', ')", linter)
  expect_no_lint("paste0(x, y, collapse = ', ')", linter)

  expect_no_lint("toString(x)", linter)

  # string match of ", " is OK -- lint only _exact_ match
  expect_no_lint('paste(x, collapse = ", \n")', linter)
})

test_that("paste_linter blocks simple disallowed usages for collapse=', '", {
  expect_lint(
    "paste(collapse = ', ', x)",
    rex::rex('toString(.) is more expressive than paste(., collapse = ", ")'),
    paste_linter()
  )
})

test_that("paste_linter respects non-default arguments", {
  expect_no_lint("paste(sep = '', 'a', 'b')", paste_linter(allow_empty_sep = TRUE))
  expect_no_lint("paste('a', 'b', sep = '')", paste_linter(allow_empty_sep = TRUE))

  expect_no_lint("paste(collapse = ', ', x)", paste_linter(allow_to_string = TRUE))
})

test_that("paste_linter works for raw strings", {
  expect_no_lint("paste(a, b, sep = R'(xyz)')", paste_linter())
  expect_lint(
    'paste(a, b, sep = R"---[]---")',
    rex::rex('paste0(...) is better than paste(..., sep = "").'),
    paste_linter()
  )

  expect_no_lint("paste(x, collapse = R'(,,)')", paste_linter())
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

  expect_no_lint("paste(x, collapse = '')", linter)
  expect_no_lint("paste(rep('*', 10), collapse = '+')", linter)
  expect_no_lint("paste(rep(c('a', 'b'), 2), collapse = '')", linter)
  expect_no_lint("paste0(rep('a', 2), 'b', collapse = '')", linter)
  # no collapse
  expect_no_lint("paste(rep('*', 10))", linter)
  # combined before aggregating
  expect_no_lint("paste(rep('*', 10), rep('x', 10), collapse = '')", linter)
})

test_that("paste_linter blocks simple disallowed usages", {
  expect_lint(
    "paste(rep('#', width), collapse='')",
    rex::rex("strrep(x, times) is better than paste"),
    paste_linter()
  )
})

test_that("paste_linter skips allowed usages for file paths", {
  linter <- paste_linter()

  expect_no_lint("paste('a', 'b', 'c')", linter)
  expect_no_lint("paste('a', 'b', 'c', sep = ',')", linter)
  expect_no_lint("paste('a', 'b', collapse = '/')", linter)
  expect_no_lint("cat(paste('a', 'b'), sep = '/')", linter)
  expect_no_lint("sep <- '/'; paste('a', sep)", linter)
  expect_no_lint("paste(sep = ',', '/', 'a')", linter)

  # paste(..., sep='/', collapse=collapse) is not a trivial swap to file.path
  expect_no_lint("paste(x, y, sep = '/', collapse = ':')", linter)

  expect_no_lint("file.path('a', 'b', 'c')", linter)

  # testing the sep starts with / is not enough
  expect_no_lint("paste('a', 'b', sep = '//')", linter)
})

test_that("paste_linter blocks simple disallowed usages for file paths", {
  linter <- paste_linter()
  lint_msg <- rex::rex("Construct file paths with file.path(...) instead of")

  expect_lint("paste(sep = '/', 'a', 'b')", lint_msg, linter)
  expect_lint("paste('a', 'b', sep = '/')", lint_msg, linter)
})

test_that("paste_linter ignores non-path cases with paste0", {
  linter <- paste_linter()

  expect_no_lint("paste0(x, y)", linter)
  expect_no_lint("paste0('abc', 'def')", linter)
  expect_no_lint("paste0('/abc', 'def/')", linter)
  expect_no_lint("paste0(x, 'def/')", linter)
  expect_no_lint("paste0('/abc', y)", linter)
  expect_no_lint("paste0(foo(x), y)", linter)
  expect_no_lint("paste0(foo(x), 'def')", linter)

  # these might be a different lint (as.character instead, e.g.) but not here
  expect_no_lint("paste0(x)", linter)
  expect_no_lint("paste0('a')", linter)
  expect_no_lint("paste0('a', 1)", linter)
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

  expect_no_lint("paste0('/', x)", linter)
  expect_no_lint("paste0(x, '/')", linter)
  expect_no_lint("paste0(x, '//hey/', y)", linter)
  expect_no_lint("paste0(x, '/hey//', y)", linter)
})

test_that("paste_linter doesn't skip all initial/terminal '/' for paths", {
  linter <- paste_linter()
  lint_msg <- rex::rex("Construct file paths with file.path(...) instead of")

  expect_lint('paste0("/abc/", "def")', lint_msg, linter)
  expect_lint('paste0("abc/", "def/")', lint_msg, linter)
})

test_that("multiple path lints are generated correctly", {
  linter <- paste_linter()

  expect_lint(
    trim_some("{
      paste(x, y, sep = '/')
      paste0(x, '/', y)
    }"),
    list(
      rex::rex('paste(..., sep = "/")'),
      rex::rex('paste0(x, "/", y, "/", z)')
    ),
    linter
  )

  # check vectorization of multiple paste0 file paths
  expect_lint(
    trim_some('{
      paste0(x, y)
      paste0("a/", x, y, "/b")
      paste0("a", "b")
      paste0("a", x)
      paste0(a, "x")
      paste0("a/", NA_character_, "/b")
      paste0("a/", "b")
      paste0("a/", "bcd//efg", "/h")
      paste0("/", a)
      paste0(a, "/")
    }'),
    list(message = rex::rex("Construct file paths with file.path(...)"), line_number = 8L),
    linter
  )
})

test_that("allow_file_path argument works", {
  expect_no_lint("paste(x, y, sep = '/')", paste_linter(allow_file_path = "always"))
})

test_that("URLs are ignored by default, linted optionally", {
  linter <- paste_linter()
  linter_url <- paste_linter(allow_file_path = "never")

  expect_no_lint("paste0('http://site.com/', x)", linter)
  expect_lint("paste0('http://site.com/', x)", rex::rex("Construct file paths with file.path(...)"), linter_url)
})

test_that("raw strings are detected in file path logic", {
  linter <- paste_linter()
  lint_msg <- rex::rex("Construct file paths with file.path(...) instead of ")

  expect_no_lint("paste0(x, R'{abc}', y)", linter)
  expect_lint("paste0(x, R'{/abc/}', y)", lint_msg, linter)
  expect_no_lint("paste(x, y, sep = R'{//}')", linter)
  expect_lint("paste(x, y, sep = R'{/}')", lint_msg, linter)
})

test_that("paste0(collapse=...) is caught", {
  linter <- paste_linter()
  lint_msg <- rex::rex("Use paste(), not paste0(), to collapse a character vector when sep= is not used.")

  expect_no_lint("paste(x, collapse = '')", linter)
  expect_no_lint("paste0(a, b, collapse = '')", linter)
  # pass-through can pass any number of arguments
  expect_no_lint("paste0(..., collapse = '')", linter)
  expect_lint("paste0(x, collapse = '')", lint_msg, linter)
  expect_lint("paste0(x, collapse = 'xxx')", lint_msg, linter)
  expect_lint("paste0(foo(x, y, z), collapse = '')", lint_msg, linter)
})

local({
  linter <- paste_linter()
  lint_msg <- rex::rex("Use paste(), not paste0(), to collapse a character vector when sep= is not used.")
  pipes <- pipes()

  patrick::with_parameters_test_that(
    "paste0(collapse=...) is caught in pipes",
    {
      expect_no_lint(sprintf('x %s paste0(y, collapse = "")', pipe), linter)
      expect_lint(sprintf('x %s paste0(collapse = "")', pipe), lint_msg, linter)
    },
    pipe = pipes,
    .test_name = pipes
  )
})

test_that("paste0(collapse=...) cases interacting with other rules are handled", {
  linter <- paste_linter()
  lint_msg <- rex::rex("Use paste(), not paste0(), to collapse a character vector when sep= is not used.")

  # multiple lints when collapse= happens to be ", "
  expect_lint(
    "paste0(foo(x), collapse = ', ')",
    list(rex::rex('toString(.) is more expressive than paste(., collapse = ", ")'), lint_msg),
    linter
  )
  expect_lint("paste0(foo(x), collapse = ', ')", lint_msg, paste_linter(allow_to_string = TRUE))

  expect_lint(
    "paste0(rep('*', 20L), collapse='')",
    list(rex::rex("strrep(x, times) is better than paste"), lint_msg),
    linter
  )

  # paste0(..., collapse=collapse) not directly mapped to file.path
  expect_lint("paste0(x, collapse = '/')", lint_msg, linter)
  expect_no_lint("paste0(x, y, collapse = '/')", linter)
})
