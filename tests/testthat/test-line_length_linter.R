test_that("returns the correct linting", {
  linter <- line_length_linter(80)
  msg <- rex("Lines should not be more than 80 characters")

  expect_lint("blah", NULL, linter)
  expect_lint(strrep("x", 80L), NULL, linter)
  expect_lint(
    strrep("x", 81L), list(
      message = msg,
      column_number = 81L
    ),
    linter
  )

  expect_lint(
    paste(rep(strrep("x", 81L), 2L), collapse = "\n"),
    list(
      list(
        message = msg,
        column_number = 81L
      ),
      list(
        message = msg,
        column_number = 81L
      )
    ),
    linter
  )

  linter <- line_length_linter(20)
  msg <- rex("Lines should not be more than 20 characters")
  expect_lint(strrep("a", 20L), NULL, linter)
  expect_lint(
    strrep("a", 22L), list(
      message = msg,
      column_number = 21L
    ),
    linter
  )

  # Don't duplicate lints
  expect_length(
    lint(
      "x <- 2 # ------------\n",
      linters = linter,
      parse_settings = FALSE
    ),
    1L
  )
})
