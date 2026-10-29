test_that("floating point comparisons are linted", {
  linter <- float_comparison_linter()
  lint_msg <- "Avoid equality comparisons with non-integer numerics"

  expect_lint("x == 3.0", lint_msg, linters = linter)
  expect_lint("3.0 == x", lint_msg, linters = linter)
  expect_lint("y == 1/10", lint_msg, linters = linter)
  expect_lint("y == -1/10", lint_msg, linters = linter)
  expect_lint("y == 1e-3", lint_msg, linters = linter)
  expect_lint("x == 3", lint_msg, linters = linter)
  expect_lint("x == c(1, y, Z)", lint_msg, linters = linter)
  expect_lint("c(1, y, Z) == x", lint_msg, linters = linter)
  expect_lint("x %in% c(1, y, Z)", lint_msg, linters = linter)
  expect_lint("c(1, y, Z) %in% x", lint_msg, linters = linter)
  expect_lint("match(x, c(1, y, Z))", lint_msg, linters = linter)
  expect_lint("match(c(1, y, Z), x)", lint_msg, linters = linter)
})

test_that("integer comparisons are not linted", {
  linter <- float_comparison_linter()

  expect_no_lint("x == 3L", linters = linter)
  expect_no_lint("x == -3L", linters = linter)
  expect_no_lint("x == 1e3L", linters = linter)
  expect_no_lint("x == 10%/%2", linters = linter)
  expect_no_lint("x == c(1L, y, Z)", linters = linter)
  expect_no_lint("c(1L, y, Z) == x", linters = linter)
  expect_no_lint("x %in% c(1L, y, Z)", linters = linter)
  expect_no_lint("c(1L, y, Z) %in% x", linters = linter)
  expect_no_lint("match(x, c(1L, y, Z))", linters = linter)
  expect_no_lint("match(c(1L, y, Z), x)", linters = linter)
  expect_no_lint("x == Inf", linters = linter)
  expect_no_lint("x == -Inf", linters = linter)
  expect_no_lint("x == TRUE", linters = linter)
  expect_no_lint("x == FALSE", linters = linter)
  expect_no_lint("match(TRUE, x)", linters = linter)
})

test_that("integer literals can be ignored when configured", {
  linter <- float_comparison_linter(lint_implicit_integer = FALSE)
  lint_msg <- "Avoid equality comparisons with non-integer numerics"

  expect_no_lint("x == 3", linters = linter)
  expect_no_lint("x == c(1, y, Z)", linters = linter)
  expect_lint("x == 3.0", lint_msg, linters = linter)
  expect_lint("x == 1e-3", lint_msg, linters = linter)
})

test_that("lint metadata points to the comparison expression", {
  linter <- float_comparison_linter()
  lint_msg <- "Avoid equality comparisons with non-integer numerics"

  expect_lint(
    "x == 3",
    list(message = lint_msg, line_number = 1L, column_number = 3L),
    linters = linter
  )
  expect_lint(
    "3 == x",
    list(message = lint_msg, line_number = 1L, column_number = 3L),
    linters = linter
  )
  expect_lint(
    "x == 1/10",
    list(message = lint_msg, line_number = 1L, column_number = 3L),
    linters = linter
  )
  expect_lint(
    "1/10 == x",
    list(message = lint_msg, line_number = 1L, column_number = 6L),
    linters = linter
  )
  expect_lint(
    "x %in% c(1, y, Z)",
    list(message = lint_msg, line_number = 1L, column_number = 3L),
    linters = linter
  )
  expect_lint(
    "match(x, c(1, y, Z))",
    list(message = lint_msg, line_number = 1L, column_number = 1L),
    linters = linter
  )
})
