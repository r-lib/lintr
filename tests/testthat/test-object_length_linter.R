test_that("returns the correct linting", {

  expect_lint("blah",
    NULL,
    object_length_linter())

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal <- 1",
    rex("Variable and function names should not be longer than 30 characters."),
    object_length_linter())

  expect_lint("very_very_very_very_long_variable_names_are_not_ideal <<- 'foo'",
    rex("Variable and function names should not be longer than 40 characters."),
    object_length_linter(length = 40))

})
