test_that("condition_call_linter skips allowed usages", {
  linter <- condition_call_linter()

  expect_lint("stop('test', call. = FALSE)", NULL, linter)

  # works even with multiple arguments
  expect_lint("stop('this is a', 'test', call. = FALSE)", NULL, linter)

  linter <- condition_call_linter(display_call = TRUE)

  expect_lint("stop('test', call. = TRUE)", NULL, linter)

  linter <- condition_call_linter(display_call = NA)

  expect_lint("stop('test', call. = TRUE)", NULL, linter)
  expect_lint("stop('test', call. = FALSE)", NULL, linter)
})

patrick::with_parameters_test_that(
  "condition_call_linter blocks disallowed usages",
  {
    linter <- condition_call_linter()
    lint_message <- rex::rex(call_name, anything, "to not display call")

    expect_lint(paste0(call_name, "('test')"), lint_message, linter)
    expect_lint(paste0(call_name, "('test', call. = TRUE)"), lint_message, linter)

    linter <- condition_call_linter(display_call = TRUE)
    lint_message <- rex::rex(call_name, anything, "to display call")

    expect_lint(paste0(call_name, "('test', call. = FALSE)"), lint_message, linter)

    linter <- condition_call_linter(display_call = NA)
    lint_message <- rex::rex("explicit value", anything, call_name)

    expect_lint(paste0(call_name, "('test')"), lint_message, linter)
  },
  call_name = c("stop", "warning")
)
