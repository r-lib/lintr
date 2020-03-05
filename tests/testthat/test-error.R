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
  expect_lint("a <- 1
    function() {
    b",
    rex("unexpected end of input"), (function(...) NULL))

  expect_lint("x=\n",
    rex("unexpected end of input")
    )
  expect_lint("sum(1:10, na.rm += 1)\n",
    rex("unexpected '='")
    )
})
