context("error")
test_that("returns the correct linting", {
  expect_lint("\"\\R\"",
    rex("is an unrecognized escape in character string starting")
    )

  expect_lint("\"\\A\"",
    rex("is an unrecognized escape in character string starting")
    )

  expect_lint("\"\\z\"",
    rex("is an unrecognized escape in character string starting")
    )
})

