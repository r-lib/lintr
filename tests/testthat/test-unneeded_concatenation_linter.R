test_that("unneeded_concatenation_linter generates defunct error", {
  expect_error(
    unneeded_concatenation_linter(),
    rex::rex("Linter unneeded_concatenation_linter was deprecated")
  )
})
