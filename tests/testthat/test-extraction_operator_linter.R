context("extraction_operator_linter")

test_that("linter returns the correct linting", {
  msg_bracket <- rex::escape("Use `[[` instead of `[`  to extract an element.")
  msg_dollar <- rex::escape("Use `[[` instead of `$`  to extract an element.")
  linter <- extraction_operator_linter()
  expect_is(linter, "linter")

  expect_lint("x[[1]]", NULL, linter)
  expect_lint("x[-1]", NULL, linter)
  expect_lint("x[1, 'a']", NULL, linter)
  expect_lint("self$a", NULL, linter)
  expect_lint(".self $\na", NULL, linter)
  expect_lint("x$a", c(message=msg_dollar, line_number=1L, column_number=2L), linter)
  expect_lint("x $\na", c(message=msg_dollar, line_number=1L, column_number=3L), linter)
  expect_lint("x[NULL]", c(message=msg_bracket, line_number=1L, column_number=2L), linter)
  expect_lint("x[++ + 3]", c(message=msg_bracket, line_number=1L, column_number=2L), linter)
  expect_lint("c(x['a'], x [ 1 ])",
              list(
                c(message=msg_bracket, line_number=1L, column_number=4L),
                c(message=msg_bracket, line_number=1L, column_number=13L)
              ),
              linter)
})


