test_that("paste_linter skips allowed usages for sep=''", {
  expect_lint("paste('a', 'b', 'c')", NULL, paste_linter())
  expect_lint("paste('a', 'b', 'c', sep = ',')", NULL, paste_linter())
  expect_lint("paste('a', 'b', collapse = '')", NULL, paste_linter())
  expect_lint("cat(paste('a', 'b'), sep = '')", NULL, paste_linter())
  expect_lint("sep <- ''; paste('a', sep)", NULL, paste_linter())
  expect_lint("paste(sep = ',', '', 'a')", NULL, paste_linter())

  expect_lint("paste0('a', 'b', 'c')", NULL, paste_linter())
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
  expect_lint("paste('a', 'b', 'c')", NULL, paste_linter())
  expect_lint("paste(x, sep = ', ')", NULL, paste_linter())
  expect_lint("paste(x, collapse = ',')", NULL, paste_linter())
  expect_lint("paste(foo(x), collapse = '/')", NULL, paste_linter())
  # harder to catch statically
  expect_lint("collapse <- ', '; paste(x, collapse = collapse)", NULL, paste_linter())

  # paste(..., sep=sep, collapse=", ") is not a trivial swap to toString
  expect_lint("paste(x, y, sep = '.', collapse = ', ')", NULL, paste_linter())
  # any call involving ...length() > 1 will implicitly use the default sep
  expect_lint("paste(x, y, collapse = ', ')", NULL, paste_linter())
  expect_lint("paste0(x, y, collapse = ', ')", NULL, paste_linter())

  expect_lint("toString(x)", NULL, paste_linter())

  # string match of ", " is OK -- lint only _exact_ match
  expect_lint('paste(x, collapse = ", \n")', NULL, paste_linter())
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
