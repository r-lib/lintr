test_that("pipe_consistency skips allowed usage", {
  skip_if_not_r_version("4.1.0")
  expect_lint("1:3 %>% mean() %>% as.character()", NULL, pipe_consistency_linter())
  expect_lint("1:3 |> mean() |> as.character()", NULL, pipe_consistency_linter())
  # With no pipes
  expect_lint("x <- 1:5", NULL, pipe_consistency_linter())
  # Across multiple lines
  expect_lint(
    c("1:3 %>%", "mean() %>%", "as.character()"),
    NULL,
    pipe_consistency_linter()
  )
})

test_that("pipe_consistency lints inconsistent usage", {
  skip_if_not_r_version("4.1.0")
  expected_msg <- rex::rex("Use consistent pipe operators (either all %>% or all |>).")
  expect_lint(
    "1:3 |> mean() %>% as.character()",
    list(
      list(message = expected_msg, line_number = 1L, column_number = 5L),
      list(message = expected_msg, line_number = 1L, column_number = 15L)
    ),
    pipe_consistency_linter()
  )

  expect_lint(
    "1:3 %>% mean() |> as.character()",
    list(
      list(message = expected_msg, line_number = 1L, column_number = 5L),
      list(message = expected_msg, line_number = 1L, column_number = 16L)
    ),
    pipe_consistency_linter()
  )

  # Across lines
  expect_lint(
    c("1:3 %>%", "mean() |>", "as.character()"),
    list(
      list(message = expected_msg, line_number = 1L, column_number = 5L),
      list(message = expected_msg, line_number = 2L, column_number = 8L)
    ),
    pipe_consistency_linter()
  )
})
