test_that("pipe_call_linter skips allowed usages", {
  linter <- pipe_call_linter()

  expect_lint("a %>% foo()", NULL, linter)
  expect_lint("a %>% foo(x)", NULL, linter)
  expect_lint("b %>% { foo(., ., .) }", NULL, linter)
  expect_lint("a %>% foo() %>% bar()", NULL, linter)

  # ensure it works across lines too
  lines <- trim_some("
  a %>%
    foo() %>%
    bar()
  ")
  expect_lint(lines, NULL, linter)

  # symbol extraction is OK (don't force extract2(), e.g.)
  expect_lint("a %>% .$y %>% mean()", NULL, linter)

  # more complicated expressions don't pick up on nested symbols
  lines <- trim_some("
  x %>% {
    tmp <- .
    bla <- foo %>% unrelated_stuff(tmp)
    my_combination_fun(tmp, bla)
  }
  ")
  expect_lint(lines, NULL, linter)

  # extraction pipe uses RHS symbols
  expect_lint("a %$% b", NULL, linter)
})

test_that("pipe_call_linter blocks simple disallowed usages", {
  expect_lint(
    "x %>% foo",
    "Use explicit calls in magrittr pipes",
    pipe_call_linter()
  )

  expect_lint(
    "x %>% foo() %>% bar",
    "Use explicit calls in magrittr pipes",
    pipe_call_linter()
  )

  expect_lint(
    "x %>% foo %>% bar()",
    "Use explicit calls in magrittr pipes",
    pipe_call_linter()
  )

  lines <- trim_some("
  a %>%
    foo %>%
    bar()
  ")
  expect_lint(
    lines,
    "Use explicit calls in magrittr pipes",
    pipe_call_linter()
  )
})

local({
  pipes <- pipes(exclude = c("%$%", "|>"))
  linter <- pipe_call_linter()
  patrick::with_parameters_test_that(
    "All pipe operators are caught",
    {
      expect_lint(sprintf("a %s foo()", pipe), NULL, linter)
      expect_lint(sprintf("a %s foo", pipe), sprintf("`a %s foo`", pipe), linter)
    },
    pipe = pipes,
    .test_name = names(pipes)
  )
})

test_that("Multiple lints give custom messages", {
  expect_lint(
    trim_some("
      a %>% b
      c %T>% d
    "),
    list(
      list(message = "%>%", line_number = 1L),
      list(message = "%T>%", line_number = 2L)
    ),
    pipe_call_linter()
  )
})
