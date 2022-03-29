test_that("paste_to_string_linter skips allowed usages", {
  expect_lint("paste('a', 'b', 'c')", NULL, paste_to_string_linter())
  expect_lint("paste(x, sep = ', ')", NULL, paste_to_string_linter())
  expect_lint("paste(x, collapse = ',')", NULL, paste_to_string_linter())
  expect_lint("paste(foo(x), collapse = '/')", NULL, paste_to_string_linter())
  # harder to catch statically
  expect_lint("collapse <- ', '; paste(x, collapse = collapse)", NULL, paste_to_string_linter())

  # paste(..., sep=sep, collapse=", ") is not a trivial swap to toString
  expect_lint("paste(x, y, sep = '.', collapse = ', ')", NULL, paste_to_string_linter())
  # any call involving ...length() > 1 will implicitly use the default sep
  expect_lint("paste(x, y, collapse = ', ')", NULL, paste_to_string_linter())
  expect_lint("paste0(x, y, collapse = ', ')", NULL, paste_to_string_linter())

  expect_lint("toString(x)", NULL, paste_to_string_linter())

  # string match of ", " is OK -- lint only _exact_ match
  expect_lint('paste(x, collapse = ", \n")', NULL, paste_to_string_linter())
})

test_that("paste_to_string_linter blocks simple disallowed usages", {
  expect_lint(
    "paste(collapse = ', ', x)",
    rex::rex('toString(.) is more expressive than paste(., collapse = ", ")'),
    paste_to_string_linter()
  )

  expect_lint(
    "paste0(foo(x), collapse = ', ')",
    rex::rex('toString(.) is more expressive than paste(., collapse = ", ")'),
    paste_to_string_linter()
  )
})
