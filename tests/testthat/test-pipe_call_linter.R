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
