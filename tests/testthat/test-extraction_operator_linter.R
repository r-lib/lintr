test_that("extraction_operator_linter generates deprecation error", {
  expect_error(
    extraction_operator_linter(),
    rex::rex("Linter extraction_operator_linter was deprecated")
  )
})
