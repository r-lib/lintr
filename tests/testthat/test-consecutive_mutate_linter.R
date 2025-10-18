test_that("consecutive_mutate_linter skips allowed usages", {
  linter <- consecutive_mutate_linter()

  expect_no_lint("DF %>% mutate(x = 1)", linter)

  # intervening expression
  expect_no_lint("DF %>% mutate(x = 1) %>% filter(x > 2) %>% mutate(y = 2)", linter)

  # pipeline starts with mutate()
  expect_no_lint("mutate(DF, x = 1) %>% arrange(y) %>% mutate(z = 2)", linter)

  # new dplyr: .keep and .by arguments are ignored
  expect_no_lint("DF %>% mutate(a = 1) %>% mutate(a = a / sum(a), .by = b)", linter)
  expect_no_lint("DF %>% mutate(a = 1) %>% mutate(a = b, .keep = 'none')", linter)
  expect_no_lint("DF %>% mutate(a = a / sum(a), .by = b) %>% mutate(c = 1)", linter)
  expect_no_lint("DF %>% mutate(a = 1, .keep = 'none') %>% mutate(a = a + 1)", linter)
})

patrick::with_parameters_test_that(
  "consecutive_mutate_linter skips files loading SQL backends",
  {
    linter <- consecutive_mutate_linter(invalid_backends = backend)

    expect_no_lint(trim_some(glue::glue("
        library({backend})
        DF %>% mutate(a = a + 1) %>% mutate(b = a - 2)
      ")), linter)

    expect_no_lint(trim_some(glue::glue("
        require('{backend}')
        DF %>% mutate(a = a + 1) %>% mutate(b = a - 2)
      ")), linter)

    expect_no_lint(trim_some(glue("
        conn %>%
          tbl({backend}::sql('SELECT 1 AS x')) %>%
          mutate(a = x + 1) %>%
          mutate(b = a + 1)
      ")), linter)

    expect_no_lint(trim_some(glue("
        conn %>%
          tbl({backend}:::sql('SELECT 1 AS x')) %>%
          mutate(a = x + 1) %>%
          mutate(b = a + 1)
      ")), linter)

    expect_no_lint(trim_some(glue("
        #' @import {backend}
        NULL

        DF %>% mutate(a = a + 1) %>% mutate(b = a - 2)
      ")), linter)

    expect_no_lint(trim_some(glue("
        #' @importFrom {backend} sql
        NULL

        conn %>%
          tbl(sql('SELECT 1 AS x')) %>%
          mutate(a = x + 1) %>%
          mutate(b = a + 1)
      ")), linter)
  },
  .test_name = c("dbplyr", "custom.backend"),
  backend = c("dbplyr", "custom.backend")
)

test_that("consecutive_mutate_linter blocks simple disallowed usages", {
  linter <- consecutive_mutate_linter()
  lint_msg <- rex::rex("Unify consecutive calls to mutate().")

  # one test of inline usage
  expect_lint("DF %>% mutate(a = 1) %>% mutate(b = 2)", lint_msg, linter)

  expect_lint(
    trim_some("
      DF %>%
       mutate(a = 1) %>%
       mutate(b = 2)
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      DF %>%
       dplyr::mutate(a = 1) %>%
       dplyr::mutate(b = 2)
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      DF %>%
       mutate(a = 1) %>%
       # a comment on b
       mutate(b = 2)
    "),
    lint_msg,
    linter
  )

  # mutate to open pipeline followed by mutate
  expect_lint("mutate(DF, x = 1) %>% mutate(x = 2)", lint_msg, linter)
})

test_that("'parallel' calls are not linted", {
  linter <- consecutive_mutate_linter()

  expect_no_lint("foo(mutate(DF1, x = 1), mutate(DF2, y = 2))", linter)

  expect_no_lint("foo(DF1 %>% mutate(x = 1), DF2 %>% mutate(y = 2))", linter)

  expect_no_lint("DF1 %>% mutate(x = 1) %>% inner_join(DF2 %>% mutate(y = 2))", linter)
})

test_that("native pipe is linted", {
  skip_if_not_r_version("4.1.0")

  linter <- consecutive_mutate_linter()
  lint_msg <- rex::rex("Unify consecutive calls to mutate().")

  expect_lint("DF |> mutate(a = 1) |> mutate(b = 2)", lint_msg, linter)
  # Ditto mixed pipes
  expect_lint("DF %>% mutate(a = 1) |> mutate(b = 2)", lint_msg, linter)
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Unify consecutive calls to mutate().")

  expect_lint(
    trim_some("
      DF %>%
        mutate(a = 1) %>%
        mutate(b = 2) %>%
        mutate(c = 3)
    "),
    list(
      list(lint_msg, line_number = 3L),
      list(lint_msg, line_number = 4L)
    ),
    consecutive_mutate_linter()
  )
})
