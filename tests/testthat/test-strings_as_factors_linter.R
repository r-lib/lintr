test_that("strings_as_factors_linter skips allowed usages", {
  expect_lint("data.frame(1:3)", NULL, strings_as_factors_linter())
  expect_lint("data.frame(x = 1:3)", NULL, strings_as_factors_linter())

  expect_lint("data.frame(x = 'a', stringsAsFactors = TRUE)", NULL, strings_as_factors_linter())
  expect_lint("data.frame(x = 'a', stringsAsFactors = FALSE)", NULL, strings_as_factors_linter())
  expect_lint("data.frame(x = c('a', 'b'), stringsAsFactors = FALSE)", NULL, strings_as_factors_linter())

  # strings in argument names to c() don't get linted
  # TODO(michaelchirico): fix this; currently generates a false positive.
  # expect_lint("data.frame(x = c('a b' = 1L, 'b c' = 2L))", NULL, strings_as_factors_linter())

  # characters supplied to row.names are not affected
  expect_lint("data.frame(x = 1:3, row.names = c('a', 'b', 'c'))", NULL, strings_as_factors_linter())

  # ambiguous cases passes
  expect_lint("data.frame(x = c(xx, 'a'))", NULL, strings_as_factors_linter())
  expect_lint("data.frame(x = c(foo(y), 'a'))", NULL, strings_as_factors_linter())
})

test_that("strings_as_factors_linter blocks simple disallowed usages", {
  msg <- "This code relies on the default value of stringsAsFactors"

  expect_lint("data.frame('a')", msg, strings_as_factors_linter())
  expect_lint("data.frame(c('a', 'b'))", msg, strings_as_factors_linter())
  expect_lint("data.frame(x = 1:5, y = 'b')", msg, strings_as_factors_linter())
  expect_lint("data.frame(x = 1:5, y, z = 'b')", msg, strings_as_factors_linter())

  # catch row.names when combined with a character vector
  expect_lint("data.frame(x = c('c', 'd', 'e'), row.names = c('a', 'b', 'c'))", msg, strings_as_factors_linter())
  expect_lint("data.frame(row.names = c('c', 'd', 'e'), x = c('a', 'b', 'c'))", msg, strings_as_factors_linter())

  # when everything is a literal, type promotion means the column is character
  expect_lint("data.frame(x = c(TRUE, 'a'))", msg, strings_as_factors_linter())
})

test_that("strings_as_factors_linters catches rep(char) usages", {
  msg <- "This code relies on the default value of stringsAsFactors"

  expect_lint("data.frame(rep('a', 10L))", msg, strings_as_factors_linter())
  expect_lint("data.frame(rep(c('a', 'b'), 10L))", msg, strings_as_factors_linter())

  # literal char, not mixed or non-char
  expect_lint("data.frame(rep(1L, 10L))", NULL, strings_as_factors_linter())
  expect_lint("data.frame(rep(c(x, 'a'), 10L))", NULL, strings_as_factors_linter())
  # however, type promotion of literals is caught
  expect_lint("data.frame(rep(c(TRUE, 'a'), 10L))", msg, strings_as_factors_linter())
})

test_that("strings_as_factors_linter catches character(), as.character() usages", {
  msg <- "This code relies on the default value of stringsAsFactors"

  expect_lint("data.frame(a = character())", msg, strings_as_factors_linter())
  expect_lint("data.frame(a = character(1L))", msg, strings_as_factors_linter())
  expect_lint("data.frame(a = as.character(x))", msg, strings_as_factors_linter())

  # but not for row.names
  expect_lint("data.frame(a = 1:10, row.names = as.character(1:10))", NULL, strings_as_factors_linter())
})

test_that("strings_as_factors_linter catches more functions with string output", {
  msg <- "This code relies on the default value of stringsAsFactors"

  expect_lint("data.frame(a = paste(1, 2, 3))", msg, strings_as_factors_linter())
  expect_lint("data.frame(a = sprintf('%d', 1:10))", msg, strings_as_factors_linter())
  expect_lint("data.frame(a = format(x, just = 'right'))", msg, strings_as_factors_linter())
  expect_lint("data.frame(a = formatC(x, format = '%d'))", msg, strings_as_factors_linter())
  expect_lint("data.frame(a = prettyNum(x, big.mark = ','))", msg, strings_as_factors_linter())
  expect_lint("data.frame(a = toString(x))", msg, strings_as_factors_linter())
  expect_lint("data.frame(a = encodeString(x))", msg, strings_as_factors_linter())
  # but not for row.names
  expect_lint("data.frame(a = 1:10, row.names = paste(1:10))", NULL, strings_as_factors_linter())
})
