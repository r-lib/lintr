test_that("numeric_leading_zero_linter skips allowed usages", {
  linter <- numeric_leading_zero_linter()

  expect_no_lint("a <- 0.1", linter)
  expect_no_lint("b <- -0.2", linter)
  expect_no_lint("c <- 3.0", linter)
  expect_no_lint("d <- 4L", linter)
  expect_no_lint("e <- TRUE", linter)
  expect_no_lint("f <- 0.5e6", linter)
  expect_no_lint("g <- 0x78", linter)
  expect_no_lint("h <- 0.9 + 0.1i", linter)
  expect_no_lint("h <- 0.9+0.1i", linter)
  expect_no_lint("h <- 0.9 - 0.1i", linter)
  expect_no_lint("i <- 2L + 3.4i", linter)
})

test_that("numeric_leading_zero_linter blocks simple disallowed usages", {
  linter <- numeric_leading_zero_linter()
  lint_msg <- rex::rex("Include the leading zero for fractional numeric constants.")

  expect_lint("a <- .1", lint_msg, linter)
  expect_lint("b <- -.2", lint_msg, linter)
  expect_lint("c <- .3 + 4.5i", lint_msg, linter)
  expect_lint("d <- 6.7 + .8i", lint_msg, linter)
  expect_lint("d <- 6.7+.8i", lint_msg, linter)
  expect_lint("e <- .9e10", lint_msg, linter)
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Include the leading zero for fractional numeric constants.")

  expect_lint(
    trim_some("{
      .1
      -.2
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    numeric_leading_zero_linter()
  )
})
