test_that("repeat_linter works as expected", {
  linter <- repeat_linter()
  msg <- rex::rex("Use 'repeat' instead of 'while (TRUE)' for infinite loops.")

  expect_lint("repeat { }", NULL, linter)
  expect_lint("while (FALSE) { }", NULL, linter)
  expect_lint("while (i < 5) { }", NULL, linter)
  expect_lint("while (j < 5) TRUE", NULL, linter)
  expect_lint("while (TRUE && j < 5) { ... }", NULL, linter)

  expect_lint("while (TRUE) { }", msg, linter)
  expect_lint("for (i in 1:10) { while (TRUE) { if (i == 5) { break } } }", msg, linter)
  expect_lint("while (TRUE) { while (TRUE) { } }", list(msg, msg), linter)
  expect_lint(
    trim_some("{
      while (TRUE) {
      }
      while (TRUE) {
      }
    }"),
    list(
      list(msg, line_number = 2L, column_number = 3L, ranges = list(c(3L, 16L))),
      list(msg, line_number = 4L, column_number = 3L, ranges = list(c(3L, 16L)))
    ),
    linter
  )

  # fix for bad logic about range start/end
  expect_lint(
    trim_some("
                 while
      (TRUE) { }
    "),
    msg,
    linter
  )
})
