test_that("nested_pipe_linter skips allowed usages", {
  linter <- nested_pipe_linter()

  expect_lint("a %>% b() %>% c()", NULL, linter)

  expect_lint(
    trim_some("
      foo <- function(x) {
        out <- a %>% b()
        return(out)
      }
    "),
    NULL,
    linter
  )

  # pipes fitting on one line can be ignored
  expect_lint(
    "bind_rows(a %>% select(b), c %>% select(b))",
    NULL,
    linter
  )

  # switch outputs are OK
  expect_lint("switch(x, a = x %>% foo())", NULL, linter)
  # final position is an output position
  expect_lint("switch(x, a = x, x %>% foo())", NULL, linter)

  # inline switch inputs are not linted
  expect_lint(
    trim_some("
      switch(
        x %>% foo(),
        a = x
      )
    "),
    NULL,
    linter
  )
})

patrick::with_parameters_test_that(
  "allow_outer_calls defaults are ignored by default",
  expect_lint(
    trim_some(sprintf(outer_call, fmt = "
      %s(
        x %%>%%
          foo()
      )
    ")),
    NULL,
    nested_pipe_linter()
  ),
  .test_name = c("try", "tryCatch", "withCallingHandlers"),
  outer_call = c("try", "tryCatch", "withCallingHandlers")
)

test_that("nested_pipe_linter blocks simple disallowed usages", {
  linter <- nested_pipe_linter()
  linter_inline <- nested_pipe_linter(allow_inline = FALSE)
  lint_msg <- rex::rex("Don't nest pipes inside other calls.")

  expect_lint(
    "bind_rows(a %>% select(b), c %>% select(b))",
    list(lint_msg, lint_msg),
    linter_inline
  )

  expect_lint(
    trim_some("
      print(
        a %>%
          filter(b > c)
      )
    "),
    lint_msg,
    linter
  )

  # switch inputs are linted
  expect_lint(
    trim_some("
      switch(
        x %>%
          foo(),
        a = x
      )
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      switch(
        x %>% foo(),
        a = x
      )
    "),
    lint_msg,
    linter_inline
  )
})

test_that("allow_outer_calls= argument works", {
  expect_lint(
    trim_some("
      try(
        x %>%
          foo()
      )
    "),
    rex::rex("Don't nest pipes inside other calls."),
    nested_pipe_linter(allow_outer_calls = character())
  )

  expect_lint(
    trim_some("
      print(
        x %>%
          foo()
      )
    "),
    NULL,
    nested_pipe_linter(allow_outer_calls = "print")
  )
})

test_that("Native pipes are handled as well", {
  skip_if_not_r_version("4.1.0")

  linter <- nested_pipe_linter()
  linter_inline <- nested_pipe_linter(allow_inline = FALSE)
  lint_msg <- rex::rex("Don't nest pipes inside other calls.")

  expect_lint(
    "bind_rows(a |> select(b), c |> select(b))",
    NULL,
    linter
  )
  expect_lint(
    "bind_rows(a |> select(b), c |> select(b))",
    list(lint_msg, lint_msg),
    linter_inline
  )

  expect_lint(
    trim_some("
      print(
        a |>
          filter(b > c)
      )
    "),
    lint_msg,
    linter
  )
})

test_that("lints vectorize", {
  lint_msg <- rex::rex("Don't nest pipes inside other calls.")

  lines <- trim_some("{
    bind_rows(
      a %>% select(b),
      c %>%
        select(d),
      e %>%
        select(f) %>%
        filter(g > 0),
      h %>% filter(i < 0)
    )
  }")
  expect_lint(
    lines,
    list(
      list(lint_msg, line_number = 4L),
      list(lint_msg, line_number = 6L)
    ),
    nested_pipe_linter()
  )

  expect_lint(
    lines,
    list(
      list(lint_msg, line_number = 3L),
      list(lint_msg, line_number = 4L),
      list(lint_msg, line_number = 6L),
      list(lint_msg, line_number = 9L)
    ),
    nested_pipe_linter(allow_inline = FALSE)
  )
})
