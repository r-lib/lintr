test_that("scalar_in_linter skips allowed usages", {
  linter <- scalar_in_linter()

  expect_lint("x %in% y", NULL, linter)
  expect_lint("y %in% c('a', 'b')", NULL, linter)
  expect_lint("c('a', 'b') %chin% x", NULL, linter)
  expect_lint("z %in% 1:3", NULL, linter)
  # scalars on LHS are fine (often used as `"col" %in% names(DF)`)
  expect_lint("3L %in% x", NULL, linter)
})

test_that("scalar_in_linter blocks simple disallowed usages", {
  linter <- scalar_in_linter()
  lint_in_msg <- rex::rex("Use == to match length-1 scalars, not %in%.")
  lint_chin_msg <- rex::rex("Use == to match length-1 scalars, not %chin%.")

  expect_lint("x %in% 1", lint_in_msg, linter)
  expect_lint("x %chin% 'a'", lint_chin_msg, linter)
})

test_that("%in% NA recommends using is.na() alone, not ==", {
  linter <- scalar_in_linter()
  lint_msg <- rex::rex("Use is.na() to check missingness, not %in%.")

  expect_lint("x %in% NA", lint_msg, linter)
  expect_lint("x %in% NA_real_", lint_msg, linter)
})

test_that("multiple lints are generated correctly", {
  linter <- scalar_in_linter()

  expect_lint(
    trim_some('{
      x %in% 1
      y %chin% "a"
    }'),
    list("%in%", "%chin%"),
    linter
  )

  expect_lint(
    trim_some("{
      x %in% NA
      x %chin% 2
      x %chin% NA_character_
      x %in% 'd'
    }"),
    list(
      rex::rex("Use is.na() to check missingness, not %in%."),
      rex::rex("Use == to match length-1 scalars, not %chin%."),
      rex::rex("Use is.na() to check missingness, not %chin%."),
      rex::rex("Use == to match length-1 scalars, not %in%.")
    ),
    linter
  )
})
