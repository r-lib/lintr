test_that("condition_call_linter skips allowed usages", {
  linter <- condition_call_linter()

  expect_no_lint("stop('test', call. = FALSE)", linter)

  # works even with multiple arguments
  expect_no_lint("stop('this is a', 'test', call. = FALSE)", linter)

  linter <- condition_call_linter(display_call = TRUE)

  expect_no_lint("stop('test', call. = TRUE)", linter)

  linter <- condition_call_linter(display_call = NA)

  expect_no_lint("stop('test', call. = TRUE)", linter)
  expect_no_lint("stop('test', call. = FALSE)", linter)
})

patrick::with_parameters_test_that(
  "condition_call_linter blocks disallowed usages",
  {
    linter <- condition_call_linter()
    lint_message <- rex::rex(call_name, anything, "not to display the call")

    expect_lint(paste0(call_name, "('test')"), lint_message, linter)
    expect_lint(paste0(call_name, "('test', call. = TRUE)"), lint_message, linter)

    linter <- condition_call_linter(display_call = TRUE)
    lint_message <- rex::rex(call_name, anything, "to display the call")

    expect_lint(paste0(call_name, "('test', call. = FALSE)"), lint_message, linter)

    linter <- condition_call_linter(display_call = NA)
    lint_message <- rex::rex("explicit value", anything, call_name)

    expect_lint(paste0(call_name, "('test')"), lint_message, linter)
  },
  call_name = c("stop", "warning")
)

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      stop(e)
      warning(w)
    }"),
    list(
      list("stop", line_number = 2L),
      list("warning", line_number = 3L)
    ),
    condition_call_linter()
  )
})
