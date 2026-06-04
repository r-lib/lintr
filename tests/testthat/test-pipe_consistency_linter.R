# fuzzer disable: pipe
test_that("pipe_consistency skips allowed usage", {
  linter <- pipe_consistency_linter()

  expect_no_lint("1:3 |> mean() |> as.character()", linter)
  # With no pipes
  expect_no_lint("x <- 1:5", linter)
  # Across multiple lines
  expect_no_lint(
    trim_some("
      1:3 |>
        mean() |>
        as.character()
    "),
    linter
  )
})

test_that("pipe_consistency lints blocked usage", {
  linter <- pipe_consistency_linter()
  lint_message <- rex::rex("Use the |> pipe operator instead of the %>% pipe operator.")


  expect_lint("1:3 %>% mean() %>% as.character()", list(lint_message, lint_message), linter)

  expect_lint(
    "1:3 |> mean() %>% as.character()",
    list(lint_message, line_number = 1L, column_number = 15L),
    linter
  )

  expect_lint(
    "1:3 %>% mean() |> as.character()",
    list(lint_message, line_number = 1L, column_number = 5L),
    linter
  )

  expect_lint(
    trim_some("
      1:3 %>%
        mean() |>
        as.character()
    "),
    list(lint_message, line_number = 1L, column_number = 5L),
    linter
  )

  expect_lint(
    "1:3 |> sort() |> mean() %>% as.character()",
    list(lint_message, line_number = 1L, column_number = 25L),
    linter
  )
})


test_that("pipe_consistency_linter works with |> argument", {
  linter <- pipe_consistency_linter(pipe = "|>")
  lint_message <- rex::rex("Use the |> pipe operator instead of the %>% pipe operator.")
  lint_message_tee <- rex::rex("Use the |> pipe operator instead of the %T>% pipe operator.")

  expect_lint(
    trim_some("
      1:3 %>%
        mean() %>%
        as.character()
    "),
    list(
      list(lint_message, line_number = 1L, column_number = 5L),
      list(lint_message, line_number = 2L, column_number = 10L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      1:3 |>
        mean() %>%
        as.character()
    "),
    list(lint_message, line_number = 2L, column_number = 10L),
    linter
  )

  expect_no_lint(
    "1:3 |> mean() |> as.character()",
    linter
  )

  expect_lint(
    trim_some("
      1:3 |>
        mean() %>%
        as.character()
    "),
    list(lint_message, line_number = 2L, column_number = 10L),
    linter
  )

  expect_lint(
    "1:3 %>% mean() %T>% print()",
    list(lint_message, lint_message_tee),
    linter
  )

  expect_lint(
    "1:3 |> mean() %T>% print()",
    list(lint_message_tee, line_number = 1L, column_number = 15L),
    linter
  )
})

test_that("pipe_consistency_linter works with %>% argument", {
  linter <- pipe_consistency_linter(pipe = "%>%")
  expected_message <- rex::rex("Use the %>% pipe operator instead of the |> pipe operator.")

  expect_lint(
    "1:3 |> mean() |> as.character()",
    list(
      list(message = expected_message, line_number = 1L, column_number = 5L),
      list(message = expected_message, line_number = 1L, column_number = 15L)
    ),
    linter
  )

  expect_lint(
    "1:3 %>% mean() |> as.character()",
    list(message = expected_message, line_number = 1L, column_number = 16L),
    linter
  )

  expect_no_lint(
    "1:3 %>% mean() %>% as.character()",
    linter
  )

  expect_lint(
    trim_some("
      1:3 %>%
        mean() |>
        as.character()
    "),
    list(message = expected_message, line_number = 2L, column_number = 10L),
    linter
  )
})

test_that("simply enforcing a consistent style is supported", {
  linter <- pipe_consistency_linter("auto")
  lint_message <- rex::rex("Stick to one pipe operator; found 1 instances of %>% and 1 instances of |>.")

  expect_no_lint("1:3 %>% mean() %>% as.character()", linter)

  expect_lint(
    "1:3 |> mean() %>% as.character()",
    list(
      list(lint_message, line_number = 1L, column_number = 5L),
      list(lint_message, line_number = 1L, column_number = 15L)
    ),
    linter
  )

  expect_lint(
    "1:3 %>% mean() |> as.character()",
    list(
      list(lint_message, line_number = 1L, column_number = 5L),
      list(lint_message, line_number = 1L, column_number = 16L)
    ),
    linter
  )

  expect_lint(
    trim_some("
      1:3 %>%
        mean() |>
        as.character()
    "),
    list(
      list(lint_message, line_number = 1L, column_number = 5L),
      list(lint_message, line_number = 2L, column_number = 10L)
    ),
    linter
  )

  lint_message_multi <- rex::rex("Stick to one pipe operator; found 1 instances of %>% and 2 instances of |>.")
  expect_lint(
    "1:3 |> sort() |> mean() %>% as.character()",
    list(
      list(lint_message_multi, line_number = 1L, column_number = 5L),
      list(lint_message_multi, line_number = 1L, column_number = 15L),
      list(lint_message_multi, line_number = 1L, column_number = 25L)
    ),
    linter
  )

  expect_no_lint("1:3 %>% mean() %T% print()", linter)
  expect_lint(
    "1:3 |> mean() %T>% print()",
    list(
      list(lint_message, line_number = 1L, column_number = 5L),
      list(lint_message, line_number = 1L, column_number = 15L)
    ),
    linter
  )
})
# fuzzer enable: pipe
