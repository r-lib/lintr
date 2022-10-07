test_that("is_numeric_linter skips allowed usages involving ||", {
  expect_lint("is.numeric(x) || is.integer(y)", NULL, is_numeric_linter())
  # x is used, but not identically
  expect_lint("is.numeric(x) || is.integer(foo(x))", NULL, is_numeric_linter())
  # not totally crazy, e.g. if input accepts a vector or a list
  expect_lint("is.numeric(x) || is.integer(x[[1]])", NULL, is_numeric_linter())
})

test_that("is_numeric_linter skips allowed usages involving %in%", {
  # false positives for class(x) %in% c('integer', 'numeric') style
  expect_lint("class(x) %in% 1:10", NULL, is_numeric_linter())
  expect_lint("class(x) %in% 'numeric'", NULL, is_numeric_linter())
  expect_lint("class(x) %in% c('numeric', 'integer', 'factor')", NULL, is_numeric_linter())
  expect_lint("class(x) %in% c('numeric', 'integer', y)", NULL, is_numeric_linter())
})

test_that("is_numeric_linter blocks disallowed usages involving ||", {
  linter <- is_numeric_linter()
  lint_msg <- rex::rex("same as is.numeric(x) || is.integer(x)")

  expect_lint("is.numeric(x) || is.integer(x)", lint_msg, linter)

  # order doesn't matter
  expect_lint("is.integer(x) || is.numeric(x)", lint_msg, linter)

  # identical expressions match too
  expect_lint("is.integer(DT$x) || is.numeric(DT$x)", lint_msg, linter)

  # line breaks don't matter
  lines <- trim_some("
    if (
      is.integer(x)
      || is.numeric(x)
    ) TRUE
  ")
  expect_lint(lines, lint_msg, linter)

  # caught when nesting
  expect_lint("all(y > 5) && (is.integer(x) || is.numeric(x))", lint_msg, linter)

  # implicit nesting
  expect_lint("is.integer(x) || is.numeric(x) || is.logical(x)", lint_msg, linter)
})

test_that("is_numeric_linter blocks disallowed usages involving %in%", {
  linter <- is_numeric_linter()
  lint_msg <- rex::rex('same as class(x) %in% c("integer", "numeric")')

  expect_lint("class(x) %in% c('integer', 'numeric')", lint_msg, linter)
  expect_lint('class(x) %in% c("numeric", "integer")', lint_msg, linter)
})

test_that("raw strings are handled properly when testing in class", {
  skip_if_not_r_version("4.0.0")

  linter <- is_numeric_linter()
  lint_msg <- rex::rex('same as class(x) %in% c("integer", "numeric")')

  expect_lint("class(x) %in% c(R'(numeric)', 'integer', 'factor')", NULL, linter)
  expect_lint("class(x) %in% c('numeric', R'--(integer)--', y)", NULL, linter)

  expect_lint("class(x) %in% c(R'(integer)', 'numeric')", lint_msg, linter)
  expect_lint('class(x) %in% c("numeric", R"--[integer]--")', lint_msg, linter)
})
