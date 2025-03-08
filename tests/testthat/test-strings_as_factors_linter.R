test_that("strings_as_factors_linter skips allowed usages", {
  linter <- strings_as_factors_linter()

  expect_no_lint("data.frame(1:3)", linter)
  expect_no_lint("data.frame(x = 1:3)", linter)

  expect_no_lint("data.frame(x = 'a', stringsAsFactors = TRUE)", linter)
  expect_no_lint("data.frame(x = 'a', stringsAsFactors = FALSE)", linter)
  expect_no_lint("data.frame(x = c('a', 'b'), stringsAsFactors = FALSE)", linter)

  # strings in argument names to c() don't get linted
  expect_no_lint("data.frame(x = c('a b' = 1L, 'b c' = 2L))", linter)

  # characters supplied to row.names are not affected
  expect_no_lint("data.frame(x = 1:3, row.names = c('a', 'b', 'c'))", linter)

  # ambiguous cases passes
  expect_no_lint("data.frame(x = c(xx, 'a'))", linter)
  expect_no_lint("data.frame(x = c(foo(y), 'a'))", linter)

  # adversarial comments
  expect_no_lint(
    trim_some("
      data.frame(
        x = 1:3,
        row.names # INJECTED COMMENT
        = c('a', 'b', 'c')
      )
    "),
    strings_as_factors_linter()
  )
})

test_that("strings_as_factors_linter blocks simple disallowed usages", {
  linter <- strings_as_factors_linter()
  lint_msg <- "Supply an explicit value for stringsAsFactors for this code"

  expect_lint("data.frame('a')", lint_msg, linter)
  expect_lint("data.frame(c('a', 'b'))", lint_msg, linter)
  expect_lint("data.frame(x = 1:5, y = 'b')", lint_msg, linter)
  expect_lint("data.frame(x = 1:5, y, z = 'b')", lint_msg, linter)

  # catch row.names when combined with a character vector
  expect_lint("data.frame(x = c('c', 'd', 'e'), row.names = c('a', 'b', 'c'))", lint_msg, linter)
  expect_lint("data.frame(row.names = c('c', 'd', 'e'), x = c('a', 'b', 'c'))", lint_msg, linter)

  # when everything is a literal, type promotion means the column is character
  expect_lint("data.frame(x = c(TRUE, 'a'))", lint_msg, linter)
})

test_that("strings_as_factors_linters catches rep(char) usages", {
  linter <- strings_as_factors_linter()
  lint_msg <- "Supply an explicit value for stringsAsFactors for this code"

  expect_lint("data.frame(rep('a', 10L))", lint_msg, linter)
  expect_lint("data.frame(rep(c('a', 'b'), 10L))", lint_msg, linter)

  # literal char, not mixed or non-char
  expect_no_lint("data.frame(rep(1L, 10L))", linter)
  expect_no_lint("data.frame(rep(c(x, 'a'), 10L))", linter)
  # however, type promotion of literals is caught
  expect_lint("data.frame(rep(c(TRUE, 'a'), 10L))", lint_msg, linter)
})

test_that("strings_as_factors_linter catches character(), as.character() usages", {
  linter <- strings_as_factors_linter()
  lint_msg <- "Supply an explicit value for stringsAsFactors for this code"

  expect_lint("data.frame(a = character())", lint_msg, linter)
  expect_lint("data.frame(a = character(1L))", lint_msg, linter)
  expect_lint("data.frame(a = as.character(x))", lint_msg, linter)

  # but not for row.names
  expect_no_lint("data.frame(a = 1:10, row.names = as.character(1:10))", linter)
})

test_that("strings_as_factors_linter catches more functions with string output", {
  linter <- strings_as_factors_linter()
  lint_msg <- "Supply an explicit value for stringsAsFactors for this code"

  expect_lint("data.frame(a = paste(1, 2, 3))", lint_msg, linter)
  expect_lint("data.frame(a = sprintf('%d', 1:10))", lint_msg, linter)
  expect_lint("data.frame(a = format(x, just = 'right'))", lint_msg, linter)
  expect_lint("data.frame(a = formatC(x, format = '%d'))", lint_msg, linter)
  expect_lint("data.frame(a = prettyNum(x, big.mark = ','))", lint_msg, linter)
  expect_lint("data.frame(a = toString(x))", lint_msg, linter)
  expect_lint("data.frame(a = encodeString(x))", lint_msg, linter)
  # but not for row.names
  expect_no_lint("data.frame(a = 1:10, row.names = paste(1:10))", linter)
})

test_that("lints vectorize", {
  lint_msg <- "Supply an explicit value for stringsAsFactors for this code"

  expect_lint(
    trim_some("{
      data.frame('a')
      data.frame('b')
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    strings_as_factors_linter()
  )
})
