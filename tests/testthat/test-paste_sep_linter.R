test_that("paste_sep_linter skips allowed usages", {
  expect_lint("paste('a', 'b', 'c')", NULL, paste_sep_linter())
  expect_lint("paste('a', 'b', 'c', sep = ',')", NULL, paste_sep_linter())
  expect_lint("paste('a', 'b', collapse = '')", NULL, paste_sep_linter())
  expect_lint("cat(paste('a', 'b'), sep = '')", NULL, paste_sep_linter())
  expect_lint("sep <- ''; paste('a', sep)", NULL, paste_sep_linter())
  expect_lint("paste(sep = ',', '', 'a')", NULL, paste_sep_linter())

  expect_lint("paste0('a', 'b', 'c')", NULL, paste_sep_linter())
})

test_that("paste_sep_linter blocks simple disallowed usages", {
  expect_lint(
    "paste(sep = '', 'a', 'b')",
    rex::rex('paste0(...) is better than paste(..., sep = "").'),
    paste_sep_linter()
  )

  expect_lint(
    "paste('a', 'b', sep = '')",
    rex::rex('paste0(...) is better than paste(..., sep = "").'),
    paste_sep_linter()
  )
})
