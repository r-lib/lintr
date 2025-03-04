test_that("boolean_arithmetic_linter doesn't block allowed usages", {
  linter <- boolean_arithmetic_linter()

  expect_no_lint("!any(x == y)", linter)
  expect_no_lint("!any(grepl(pattern, x))", linter)
})

test_that("boolean_arithmetic_linter requires use of any() or !any() over length(.(<logical>))", {
  linter <- boolean_arithmetic_linter()
  lint_msg <- rex::rex("Use any() to express logical aggregations.")

  expect_lint("length(which(x == y)) == 0", lint_msg, linter)
  # anything passed to which() can be assumed to be logical
  expect_lint("length(which(is_treatment)) == 0L", lint_msg, linter)
  # regex version
  expect_lint("length(grep(pattern, x)) == 0", lint_msg, linter)

  # non-== comparisons
  expect_lint("length(which(x == y)) > 0L", lint_msg, linter)
  expect_lint("length(which(is_treatment)) < 1", lint_msg, linter)
  expect_lint("length(grep(pattern, x)) >= 1L", lint_msg, linter)
})

local({
  linter <- boolean_arithmetic_linter()
  lint_msg <- rex::rex("Use any() to express logical aggregations.")

  outer_comparisons <- c("== 0", "== 0L", "> 0L", "> 0L", ">= 1", ">= 1L")

  patrick::with_parameters_test_that(
    "sum(x {op} y) {outer} lints",
    expect_lint(sprintf("sum(x %s y) %s", op, outer), lint_msg, linter),
    .cases = expand.grid(
      op = c("==", "!=", ">", "<", ">=", "<=", "&", "|", "%in%", "%chin%"),
      outer = outer_comparisons
    )
  )

  patrick::with_parameters_test_that(
    "sum({op}(x)) == 0 lints",
    expect_lint(sprintf("sum(%s(x)) == 0", op), lint_msg, linter),
    .cases = expand.grid(
      op = c(
        "!", "xor", "grepl", "str_detect", "is.element",
        "is.na", "is.finite", "is.infinite", "is.nan",
        "duplicated", "nzchar", "startsWith", "endsWith"
      ),
      outer = outer_comparisons
    )
  )
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
