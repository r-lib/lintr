context("spaces_inside_linter")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, spaces_inside_linter)

  expect_lint("print(blah)", NULL, spaces_inside_linter)

  expect_lint("base::print(blah)", NULL, spaces_inside_linter)

  expect_lint("a[, ]", NULL, spaces_inside_linter)

  expect_lint("a[,]", NULL, spaces_inside_linter)

  expect_lint("a[1]", NULL, spaces_inside_linter)

  expect_lint("fun(\na[1]\n  )", NULL, spaces_inside_linter)

  expect_lint("a[1 ]",
    rex("Do not place spaces around code in parentheses or square brackets."),
    spaces_inside_linter)

  expect_lint("a[ 1]",
    rex("Do not place spaces around code in parentheses or square brackets."),
    spaces_inside_linter)

  expect_lint("a[ 1 ]",
    list("Do not place spaces around code in parentheses or square brackets.",
    rex("Do not place spaces around code in parentheses or square brackets.")),
    spaces_inside_linter)

  expect_lint("a(, )", NULL, spaces_inside_linter)

  expect_lint("a(,)", NULL, spaces_inside_linter)

  expect_lint("a(1)", NULL, spaces_inside_linter)

  expect_lint("a(1 )",
    rex("Do not place spaces around code in parentheses or square brackets."),
    spaces_inside_linter)

  expect_lint("a( 1)",
    rex("Do not place spaces around code in parentheses or square brackets."),
    spaces_inside_linter)

  expect_lint("a( 1 )",
    list("Do not place spaces around code in parentheses or square brackets.",
    rex("Do not place spaces around code in parentheses or square brackets.")),
    spaces_inside_linter)

  expect_lint("\"a( 1 )\"",
    NULL,
    spaces_inside_linter)
})
