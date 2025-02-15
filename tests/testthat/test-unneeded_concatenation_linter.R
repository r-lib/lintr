test_that("unneeded_concatenation_linter generates deprecation warning", {
  expect_warning(
    unneeded_concatenation_linter(),
    rex::rex("Linter unneeded_concatenation_linter was deprecated")
  )
})
