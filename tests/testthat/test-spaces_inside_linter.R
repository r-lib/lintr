context("spaces_inside_linter")

test_that("returns the correct linting", {
  msg <- rex::rex("Do not place spaces around code in parentheses or square brackets.")
  linter <- spaces_inside_linter()
  expect_is(linter, "linter")

  expect_lint("blah", NULL, linter)

  expect_lint("print(blah)", NULL, linter)

  expect_lint("base::print(blah)", NULL, linter)

  expect_lint("a[, ]", NULL, linter)

  expect_lint("a[,]", NULL, linter)

  expect_lint("a[1]", NULL, linter)

  expect_lint("fun(\na[1]\n  )", NULL, linter)

  expect_lint("a[1 ]",
    c(message = msg, line_number = 1, column_number = 4, type = "style"),
    linter)

  expect_lint("\n\na[ 1]",
    c(message = msg, line_number = 3, column_number = 3, type = "style"),
    linter)

  expect_lint("a[ 1 ]",
    list(c(message = msg, line_number = 1, column_number = 3, type = "style"),
         c(message = msg, line_number = 1, column_number = 5, type = "style")),
    linter)

  expect_lint("a(, )", NULL, linter)

  expect_lint("a(,)", NULL, linter)

  expect_lint("a(1)", NULL, linter)

  expect_lint("a(1 )",
    c(message = msg, line_number = 1, column_number = 4, type = "style"),
    linter)

  expect_lint("a( 1)",
    c(message = msg, line_number = 1, column_number = 3, type = "style"),
    linter)

  expect_lint("a( 1 )",
    list(c(message = msg, line_number = 1, column_number = 3, type = "style"),
         c(message = msg, line_number = 1, column_number = 5, type = "style")),
    linter)

  expect_lint("\"a( 1 )\"",
    NULL,
    linter)

})
