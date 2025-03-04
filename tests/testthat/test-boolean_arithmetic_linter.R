test_that("boolean_arithmetic_linter doesn't block allowed usages", {
  linter <- boolean_arithmetic_linter()

  expect_no_lint("!any(x == y)", linter)
  expect_no_lint("!any(grepl(pattern, x))", linter)
})

test_that("boolean_arithmetic_linter requires use of any() or !any()", {
  linter <- boolean_arithmetic_linter()
  lint_msg <- rex::rex("Use any() to express logical aggregations.")

  expect_lint("length(which(x == y)) == 0", lint_msg, linter)
  # anything passed to which() can be assumed to be logical
  expect_lint("length(which(is_treatment)) == 0L", lint_msg, linter)
  # regex version
  expect_lint("length(grep(pattern, x)) == 0", lint_msg, linter)
  # sum version
  expect_lint("sum(x == y) == 0L", lint_msg, linter)
  expect_lint("sum(x != y) == 0L", lint_msg, linter)
  expect_lint("sum(x %in% y) == 0L", lint_msg, linter)
  expect_lint("sum(x & y) == 0L", lint_msg, linter)
  expect_lint("sum(x | y) == 0L", lint_msg, linter)
  expect_lint("sum(x > y) == 0L", lint_msg, linter)
  expect_lint("sum(x < y) == 0L", lint_msg, linter)
  expect_lint("sum(x >= y) == 0L", lint_msg, linter)
  expect_lint("sum(x <= y) == 0L", lint_msg, linter)
  expect_lint("sum(xor(x, y)) == 0L", lint_msg, linter)
  expect_lint("sum(!x) == 0L", lint_msg, linter)
  expect_lint("sum(grepl(pattern, x)) == 0", lint_msg, linter)
  expect_lint("sum(str_detect(x, pattern)) == 0", lint_msg, linter)
  expect_lint("sum(is.na(x)) == 0", lint_msg, linter)
  expect_lint("sum(is.nan(x)) == 0", lint_msg, linter)
  expect_lint("sum(is.finite(x)) == 0", lint_msg, linter)
  expect_lint("sum(is.element(x)) == 0", lint_msg, linter)
  expect_lint("sum(duplicated(x)) == 0", lint_msg, linter)
  expect_lint("sum(nzchar(x)) == 0", lint_msg, linter)
  expect_lint("sum(startsWith(x)) == 0", lint_msg, linter)
  expect_lint("sum(endsWith(x)) == 0", lint_msg, linter)

  # non-== comparisons
  expect_lint("length(which(x == y)) > 0L", lint_msg, linter)
  expect_lint("length(which(is_treatment)) < 1", lint_msg, linter)
  expect_lint("length(grep(pattern, x)) >= 1L", lint_msg, linter)
  expect_lint("sum(x == y) != 0", lint_msg, linter)
  expect_lint("sum(grepl(pattern, x)) > 0L", lint_msg, linter)
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Use any() to express logical aggregations.")

  expect_lint(
    trim_some("{
      length(which(x == y)) > 0L
      sum(x == y) != 0
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    boolean_arithmetic_linter()
  )
})
