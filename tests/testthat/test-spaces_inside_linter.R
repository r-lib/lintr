test_that("returns the correct linting", {
  linter <- spaces_inside_linter()
  msg <- rex::rex("Do not place spaces around code in parentheses or square brackets.")

  expect_lint("blah", NULL, linter)
  expect_lint("print(blah)", NULL, linter)
  expect_lint("base::print(blah)", NULL, linter)
  expect_lint("a[, ]", NULL, linter)
  expect_lint("a[, ]", NULL, linter)
  expect_lint("a[1]", NULL, linter)
  expect_lint("fun(\na[1]\n  )", NULL, linter)

  expect_lint(
    "a[1 ]",
    list(message = msg, line_number = 1L, column_number = 4L, type = "style"),
    linter
  )

  expect_lint(
    "\n\na[ 1]",
    list(message = msg, line_number = 3L, column_number = 3L, type = "style"),
    linter
  )

  expect_lint(
    "a[ 1 ]",
    list(list(message = msg, line_number = 1L, column_number = 3L, type = "style"),
         list(message = msg, line_number = 1L, column_number = 5L, type = "style")),
    linter
  )

  expect_lint("a(, )", NULL, linter)

  expect_lint("a(,)", NULL, linter)

  expect_lint("a(1)", NULL, linter)

  expect_lint(
    "a(1 )",
    list(message = msg, line_number = 1L, column_number = 4L, type = "style"),
    linter
  )

  expect_lint(
    "a( 1)",
    list(message = msg, line_number = 1L, column_number = 3L, type = "style"),
    linter
  )

  expect_lint(
    "a( 1 )",
    list(list(message = msg, line_number = 1L, column_number = 3L, type = "style"),
         list(message = msg, line_number = 1L, column_number = 5L, type = "style")),
    linter
  )

  expect_lint("\"a( 1 )\"", NULL, linter)

  # trailing comments are OK (#636)
  expect_lint("or( #code\n  x, y\n)", NULL, linter)

  expect_lint(trim_some("
    fun(      # this is another comment
      a = 42, # because 42 is always the answer
      b = Inf
    )
  "), NULL, linter)
})
