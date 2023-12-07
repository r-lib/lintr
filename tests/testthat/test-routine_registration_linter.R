patrick::with_parameters_test_that(
  "lints correctly",
  {
    linter <- routine_registration_linter()
    expect_lint(sprintf("%s(ROUTINE, 1)", caller), NULL, linter)
    expect_lint(
      sprintf("%s('ROUTINE', PACKAGE = 'foo')", caller),
      "Register your native code routines with useDynLib",
      linter
    )
  },
  .test_name = c(".C", ".Call", ".External", ".Fortran"),
  caller = c(".C", ".Call", ".External", ".Fortran")
)

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    linter
  )
})
