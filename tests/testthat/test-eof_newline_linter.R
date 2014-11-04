context("eof_newline_linter")
test_that("returns the correct linting", {
  expect_lint("blah\n",
    NULL,
    eof_newline_linter)

  expect_lint("blah",
    rex("Last line should end with a newline."),
    eof_newline_linter)

  expect_lint("blah <- 1  ",
    rex("Last line should end with a newline."),
    eof_newline_linter)

  expect_lint("blah <- 1  \n'hi'",
    rex("Last line should end with a newline."),
    eof_newline_linter)

  expect_lint("blah <- 1  \n'hi'\n",
    NULL,
    eof_newline_linter)
})
