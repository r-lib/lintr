test_that("returns the correct linting", {
  linter <- spaces_inside_linter()

  expect_lint("blah", NULL, linter)
  expect_lint("print(blah)", NULL, linter)
  expect_lint("base::print(blah)", NULL, linter)
  expect_lint("a[, ]", NULL, linter)
  expect_lint("a[1]", NULL, linter)
  expect_lint("fun(\na[1]\n  )", NULL, linter)

  expect_lint(
    "a[1 ]",
    list(message = "Do not place spaces before square brackets", line_number = 1L, column_number = 4L, type = "style"),
    linter
  )

  expect_lint(
    "\n\na[ 1]",
    list(message = "Do not place spaces after square brackets", line_number = 3L, column_number = 3L, type = "style"),
    linter
  )

  expect_lint(
    "a[ 1 ]",
    list(
      list(message = "Do not place spaces after square brackets", line_number = 1L, column_number = 3L, type = "style"),
      list(message = "Do not place spaces before square brackets", line_number = 1L, column_number = 5L, type = "style")
    ),
    linter
  )

  expect_lint("a(, )", NULL, linter)

  expect_lint("a(,)", NULL, linter)

  expect_lint("a(1)", NULL, linter)

  expect_lint(
    "a(1 )",
    list(message = "Do not place spaces before parentheses", line_number = 1L, column_number = 4L, type = "style"),
    linter
  )

  expect_lint(
    "a( 1)",
    list(message = "Do not place spaces after parentheses", line_number = 1L, column_number = 3L, type = "style"),
    linter
  )

  expect_lint(
    "a( 1 )",
    list(
      list(message = "Do not place spaces after parentheses", line_number = 1L, column_number = 3L, type = "style"),
      list(message = "Do not place spaces before parentheses", line_number = 1L, column_number = 5L, type = "style")
    ),
    linter
  )

  # range covers all whitespace
  expect_lint(
    "a(  blah  )",
    list(
      list(message = "Do not place spaces after parentheses", line_number = 1L, column_number = 3L,
           ranges = list(c(3L, 4L)), type = "style"),
      list(message = "Do not place spaces before parentheses", line_number = 1L, column_number = 9L,
           ranges = list(c(9L, 10L)), type = "style")
    ),
    linter
  )

  expect_lint('"a( 1 )"', NULL, linter)

  # trailing comments are OK (#636)
  expect_lint(
    trim_some("
      or( #code
        x, y
      )
    "),
    NULL,
    linter
  )

  expect_lint(
    trim_some("
      fun(      # this is another comment
        a = 42, # because 42 is always the answer
        b = Inf
      )
    "),
    NULL,
    linter
  )
})
