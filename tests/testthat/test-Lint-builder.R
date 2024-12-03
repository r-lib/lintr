test_that("Lint() errors on invalid input", {
  dummy_line <- "abc"
  expect_error(
    Lint("dummy.R", line = dummy_line, column_number = NA_integer_),
    "`column_number` must be an integer between 0 and 4 (`nchar(line) + 1`)",
    fixed = TRUE
  )
  expect_error(
    Lint("dummy.R", line = dummy_line, line_number = 0L),
    "`line_number` must be a positive integer",
    fixed = TRUE
  )
  expect_error(
    Lint("dummy.R", ranges = c(1L, 3L)),
    "`ranges` must be `NULL` or a list",
    fixed = TRUE
  )
  expect_error(
    Lint("dummy.R", ranges = list(1L)),
    "`ranges` must only contain integer vectors of length 2 without `NA`s.",
    fixed = TRUE
  )
  expect_error(
    Lint("dummy.R", ranges = list(c(1L, NA_integer_))),
    "`ranges` must only contain integer vectors of length 2 without `NA`s.",
    fixed = TRUE
  )
  expect_error(
    Lint("dummy.R", line = dummy_line, ranges = list(c(1L, 2L), c(1L, 5L))),
    "`ranges` must satisfy 0 <= range[1L] <= range[2L] <= 4 (nchar(line) + 1).",
    fixed = TRUE
  )
})
