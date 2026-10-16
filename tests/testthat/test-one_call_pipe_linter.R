test_that("one_call_pipe_linter skips allowed usages", {
  linter <- one_call_pipe_linter()

  # two pipe steps is OK
  expect_no_lint("x %>% foo() %>% bar()", linter)
  # call in first step --> OK
  expect_no_lint("foo(x) %>% bar()", linter)
  # both calls in second step --> OK
  expect_no_lint("x %>% foo(bar(.))", linter)
  expect_no_lint("x %>% { foo(bar(.)) }", linter)
  expect_no_lint("x %>% { foo(.) + bar(.) }", linter)
  expect_no_lint(R"(x |> (\(y) foo(bar(y)))())", linter)

  # assignment pipe is exempted
  expect_no_lint("x %<>% as.character()", linter)
})

# fuzzer disable: pipe
test_that("one_call_pipe_linter blocks simple disallowed usages", {
  linter <- one_call_pipe_linter()
  lint_msg <- rex::rex("Avoid pipe %>% for expressions with only a single call.")

  expect_lint("x %>% foo()", lint_msg, linter)

  # new lines don't matter
  expect_lint("x %>%\n  foo()", lint_msg, linter)

  # nested cases (#2488)
  expect_lint("x %>% inner_join(filter(y, is_treatment))", lint_msg, linter)
  expect_lint("x %>% filter(. > 0 & y == max(z))", lint_msg, linter)
  expect_lint(
    "x %>% inner_join(y %>% filter(is_treatment))",
    list(
      list(lint_msg, line_number = 1L, column_number = 1L),
      list(lint_msg, line_number = 1L, column_number = 18L)
    ),
    linter
  )
  expect_lint(
    "x %>% inner_join(y %>% filter(., is_treatment))",
    list(
      list(lint_msg, line_number = 1L, column_number = 1L),
      list(lint_msg, line_number = 1L, column_number = 18L)
    ),
    linter
  )
  expect_lint(
    "x %>% inner_join(y %>% .[is_treatment, sum(z)])",
    list(
      list(lint_msg, line_number = 1L, column_number = 1L),
      list(lint_msg, line_number = 1L, column_number = 18L)
    ),
    linter
  )
  expect_lint(
    "x %>% inner_join(y %>% mutate(z = foo(.)))",
    list(lint_msg, line_number = 1L, column_number = 1L),
    linter
  )

  skip_unless_r(">= 4.2.0")
  expect_lint(
    "x |> inner_join(y |> filter(by = _))",
    list(
      list(rex::rex("Avoid pipe |>"), line_number = 1L, column_number = 1L),
      list(rex::rex("Avoid pipe |>"), line_number = 1L, column_number = 17L)
    ),
    linter
  )
})
# fuzzer enable: pipe

test_that("one_call_pipe_linter skips data.table chains", {
  linter <- one_call_pipe_linter()
  lint_msg <- rex::rex("Avoid pipe %>% for expressions with only a single call.")

  expect_no_lint("DT[x > 5, sum(y), by = keys] %>% .[, .SD[1], by = key1]", linter)

  # lint here: instead of a pipe, use DT[x > 5, sum(y), by = keys]
  expect_lint("DT %>% .[x > 5, sum(y), by = keys]", lint_msg, linter)

  # ditto for [[
  expect_no_lint("DT %>% rowSums() %>% .[[idx]]", linter)

  expect_lint("DT %>% .[[idx]]", lint_msg, linter)
})

test_that("one_call_pipe_linter treats all pipes equally", {
  linter <- one_call_pipe_linter()
  lint_msg_part <- " for expressions with only a single call."

  expect_no_lint("foo %>% bar() %$% col", linter)
  expect_lint("x %T>% foo()", rex::rex("%T>%", lint_msg_part), linter)
  expect_lint("x %$%\n  foo", rex::rex("%$%", lint_msg_part), linter)
  expect_no_lint('data %>% filter(type == "console") %$% obscured_id %>% unique()', linter)
})

test_that("multiple lints are generated correctly", { # nofuzz: pipe
  expect_lint(
    trim_some("{
      a %>% b()
      c %$% d
      e %T>%
        f()
    }"),
    list(
      list(rex::rex("pipe %>%"), line_number = 2L),
      list(rex::rex("pipe %$%"), line_number = 3L),
      list(rex::rex("pipe %T>%"), line_number = 4L)
    ),
    one_call_pipe_linter()
  )
})

test_that("Native pipes are handled as well", {
  linter <- one_call_pipe_linter()

  expect_lint( # nofuzz: pipe
    "x |> foo()",
    rex::rex("Avoid pipe |> for expressions with only a single call."),
    linter
  )

  # mixed pipes
  expect_no_lint("x |> foo() %>% bar()", linter)
  expect_no_lint("x %>% foo() |> bar()", linter)

  expect_lint( # nofuzz: pipe
    trim_some("{
      a %>% b()
      c |> d()
    }"),
    list(
      list(message = "pipe %>%"),
      list(message = "pipe |>")
    ),
    linter
  )
})

test_that("one_call_pipe_linter skips data.table chains with native pipe", {
  skip_unless_r(">= 4.3.0")

  linter <- one_call_pipe_linter()
  lint_msg <- rex::rex("Avoid pipe |> for expressions with only a single call.")

  expect_no_lint("DT[x > 5, sum(y), by = keys] |> _[, .SD[1], by = key1]", linter)

  # lint here: instead of a pipe, use DT[x > 5, sum(y), by = keys]
  expect_lint("DT |> _[x > 5, sum(y), by = keys]", lint_msg, linter)

  # ditto for [[
  expect_no_lint("DT |> rowSums() |> _[[idx]]", linter)

  expect_lint("DT |> _[[idx]]", lint_msg, linter)
})
