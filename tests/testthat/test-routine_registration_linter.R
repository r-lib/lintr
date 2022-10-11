test_that("routine_registration_linter skips allowed usages", {
  expect_lint(".Call(ROUTINE, 1)", NULL, routine_registration_linter())
  expect_lint(".Fortran(f_ROUTINE, 2)", NULL, routine_registration_linter())
})

skip_if_not_installed("patrick")
patrick::with_parameters_test_that(
  "unregistered routines lint",
  expect_lint(
    sprintf("%s('ROUTINE', PACKAGE = 'foo')", caller),
    "Register your native code routines with useDynLib",
    routine_registration_linter()
  ),
  .test_name = c(".C", ".Call", ".External", ".Fortran"),
  caller = c(".C", ".Call", ".External", ".Fortran")
)
