context("spaces_inside_linter")

test_that("returns the correct linting", {
  msg <- rex::rex("Do not place spaces around code in parentheses or square brackets.")

  expect_lint("blah", NULL, spaces_inside_linter)

  expect_lint("print(blah)", NULL, spaces_inside_linter)

  expect_lint("base::print(blah)", NULL, spaces_inside_linter)

  expect_lint("a[, ]", NULL, spaces_inside_linter)

  expect_lint("a[,]", NULL, spaces_inside_linter)

  expect_lint("a[1]", NULL, spaces_inside_linter)

  expect_lint("fun(\na[1]\n  )", NULL, spaces_inside_linter)

  expect_lint("a[1 ]",
    list(message = msg, line_number = 1, column_number = 4, type = "style"),
    spaces_inside_linter)

  expect_lint("\n\na[ 1]",
              list(message = msg, line_number = 3, column_number = 3, type = "style"),
    spaces_inside_linter)

  expect_lint("a[ 1 ]",
    list(list(message = msg, line_number = 1, column_number = 3, type = "style"),
         list(message = msg, line_number = 1, column_number = 5, type = "style")),
    spaces_inside_linter)

  expect_lint("a(, )", NULL, spaces_inside_linter)

  expect_lint("a(,)", NULL, spaces_inside_linter)

  expect_lint("a(1)", NULL, spaces_inside_linter)

  expect_lint("a(1 )",
    list(message = msg, line_number = 1, column_number = 4, type = "style"),
    spaces_inside_linter)

  expect_lint("a( 1)",
    list(message = msg, line_number = 1, column_number = 3, type = "style"),
    spaces_inside_linter)

  expect_lint("a( 1 )",
    list(list(message = msg, line_number = 1, column_number = 3, type = "style"),
         list(message = msg, line_number = 1, column_number = 5, type = "style")),
    spaces_inside_linter)

  expect_lint("\"a( 1 )\"",
    NULL,
    spaces_inside_linter)

})
