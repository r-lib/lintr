test_that("Lint() errors on invalid input", {
  dummy_line <- "abc"
  expect_error(
    Lint("dummy.R", line = dummy_line, column_number = NA_integer_),
    rex::rex("`column_number` must be an integer between 0 and nchar(line) + 1 (4). It was NA.")
  )
  expect_error(
    Lint("dummy.R", line = dummy_line, line_number = 0L),
    rex::rex("`line_number` must be a positive integer. It was 0.")
  )
  expect_error(
    Lint("dummy.R", ranges = c(1L, 3L)),
    rex::rex("`ranges` must be NULL or a list.")
  )
  expect_error(
    Lint("dummy.R", ranges = list(1L)),
    rex::rex("`ranges` must only contain length 2 integer vectors without NAs.")
  )
  expect_error(
    Lint("dummy.R", ranges = list(c(1L, NA_integer_))),
    rex::rex("`ranges` must only contain length 2 integer vectors without NAs.")
  )
  expect_error(
    Lint("dummy.R", line = dummy_line, ranges = list(c(1L, 2L), c(1L, 5L))),
    rex::rex("All entries in `ranges` must satisfy 0 <= range[1L] <= range[2L] <= nchar(line) + 1 (4).")
  )
})
