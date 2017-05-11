context("object_length_linter")

test_that("returns the correct linting", {
  msg <- rex("Variable and function names should not be longer than 20 characters.")
  linter <- object_length_linter()
  expect_is(linter, "linter")

  expect_lint("blah", NULL, linter)

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal <- 1",
    msg,
    linter)

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal <<- 'foo'",
    rex("Variable and function names should not be longer than 40 characters."),
    object_length_linter(length = 40))

})
