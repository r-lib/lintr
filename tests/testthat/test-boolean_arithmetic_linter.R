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
  expect_lint("sum(grepl(pattern, x)) == 0", lint_msg, linter)

  # non-== comparisons
  expect_lint("length(which(x == y)) > 0L", lint_msg, linter)
  expect_lint("length(which(is_treatment)) < 1", lint_msg, linter)
  expect_lint("length(grep(pattern, x)) >= 1L", lint_msg, linter)
  expect_lint("sum(x == y) != 0", lint_msg, linter)
  expect_lint("sum(grepl(pattern, x)) > 0L", lint_msg, linter)
})

# TODO(michaelchirico): activate these; see code comments explaining the complication.
# test_that("boolean_arithmetic_linter blocks comparisons to the object length", {
#   expect_lint(
#     "length(which(condition)) < length(condition)",
#     "grep\\(pattern, x\\) is better than which\\(grepl\\(pattern, x\\)\\)\\.",
#     boolean_arithmetic_linter
#   )
#   expect_lint(
#     "length(which(is_treatment)) == length(is_treatment)",
#     "grep\\(pattern, x\\) is better than which\\(grepl\\(pattern, x\\)\\)\\.",
#     boolean_arithmetic_linter
#   )
#   expect_lint(
#     "length(grep(pattern, x)) < length(x)",
#     "grep\\(pattern, x\\) is better than which\\(grepl\\(pattern, x\\)\\)\\.",
#     boolean_arithmetic_linter
#   )
#   expect_lint(
#     "sum(x == y) < length(x)",
#     "grep\\(pattern, x\\) is better than which\\(grepl\\(pattern, x\\)\\)\\.",
#     boolean_arithmetic_linter
#   )
#   expect_lint(
#     "sum(grepl(pattern, x)) == length(x)",
#     "grep\\(pattern, x\\) is better than which\\(grepl\\(pattern, x\\)\\)\\.",
#     boolean_arithmetic_linter
#   )
# })
