test_that("returns the correct linting", {

  expect_lint("blah",
    NULL,
    line_length_linter(80))

  expect_lint("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    NULL,
    line_length_linter(80))

  expect_lint("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    "Lines should not be more than 80 characters",
    line_length_linter(80))

  expect_lint(
    paste0("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n",
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),

    list(
      rex("Lines should not be more than 80 characters"),
      rex("Lines should not be more than 80 characters")),

    line_length_linter(80))

  expect_lint("aaaaaaaaaaaaaaaaaaaa",
    NULL,
    line_length_linter(20))

  expect_lint("aaaaaaaaaaaaaaaaaaaab",
    rex("Lines should not be more than 20 characters"),
    line_length_linter(20))

  # Don't duplicate lints
  expect_length(
    lint("x <- 2 # ------------\n",
         linters = line_length_linter(20)),
    1L
  )
})
