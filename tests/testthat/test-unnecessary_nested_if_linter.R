test_that("unnecessary_nested_if_linter generates deprecation error", {
  expect_error(
    unnecessary_nested_if_linter(),
    rex::rex("unnecessary_nested_if_linter was deprecated", anything, "Use unnecessary_nesting_linter")
  )
})
