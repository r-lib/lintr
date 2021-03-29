test_that("single numerical constants are properly identified ", {
  # Test single numerical constants
  is_implicit <- lintr:::is_implicit_integer

  x <- c("Inf", "NaN", "TRUE", "FALSE", "NA",  "NA_character")
  y <- c(FALSE, FALSE, FALSE,  FALSE,   FALSE, FALSE)
  expect_equal(is_implicit(x), y)

  x <- c("2.000", "2.",  "2L",  "2.0", "2.1", "2")
  y <- c(FALSE,   FALSE, FALSE, FALSE, FALSE, TRUE)
  expect_equal(is_implicit(x), y)

  #       1000   1000L     1000L    1200*    0.0012   0.001   0.0...      1.2
  x <- c("1e3", "1e3L", "1.0e3L", "1.2e3", "1.2e-3", "1e-3", "1e-33", "1.2e0")
  y <- c(TRUE,  FALSE,  FALSE,    FALSE,   FALSE,    FALSE,  FALSE,   FALSE)
  expect_equal(is_implicit(x), y)

  #            1*          123L         123*                   123.1
  x <- c("0x1p+0", "0x1.ecp+6L", "0x1.ecp+6", "0x1.ec66666666666p+6")
  y <- c(FALSE,    FALSE,        FALSE,       FALSE)
  expect_equal(is_implicit(x), y)

  x <- c("8i", "8.0i")
  y <- c(FALSE, FALSE)
  expect_equal(is_implicit(x), y)

  max <- .Machine[["integer.max"]]  # largest number that R can represent as an integer
  x <- as.character(c(-max - 1.0, -max, max,  max + 1.0))
  y <-              c(FALSE,      TRUE, TRUE, FALSE)
  expect_equal(is_implicit(x), y)

  # Note: cases indicated by "*" should be TRUE but they are complicated to handle, and it is not
  # clear how users could keep these whole numbers represented as doubles without a lint.
})

test_that("linter returns the correct linting", {
  msg <- "Integers should not be implicit. Use the form 1L for integers or 1.0 for doubles."
  linter <- implicit_integer_linter()
  expect_is(linter, "linter")

  expect_lint("x <<- 1L", NULL, linter)
  expect_lint("1.0/-Inf -> y", NULL, linter)
  expect_lint("y <- 1+i", list(message = msg, line_number = 1L, column_number = 7L), linter)
  expect_lint("z <- 1e5", list(message = msg, line_number = 1L, column_number = 9L), linter)
  expect_lint("cat(1:n)", list(message = msg, line_number = 1L, column_number = 6L), linter)
  expect_lint("552^9",
              list(
                list(message = msg, line_number = 1L, column_number = 4L),
                list(message = msg, line_number = 1L, column_number = 6L)
              ),
              linter)
})
