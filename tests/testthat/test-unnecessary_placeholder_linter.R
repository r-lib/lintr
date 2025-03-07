# nofuzz start
linter <- unnecessary_placeholder_linter()
pipes <- pipes(exclude = "|>")

patrick::with_parameters_test_that(
  "unnecessary_placeholder_linter skips allowed usages",
  {
    # . used in position other than first --> ok
    expect_no_lint(sprintf("x %s foo(y, .)", pipe), linter)
    # ditto for nested usage
    expect_no_lint(sprintf("x %s foo(y, bar(.))", pipe), linter)
    # . used twice --> ok
    expect_no_lint(sprintf("x %s foo(., .)", pipe), linter)
    # . used as a keyword argument --> ok
    expect_no_lint(sprintf("x %s foo(arg = .)", pipe), linter)
    # . used inside a scope --> ok
    expect_no_lint(sprintf("x %s { foo(arg = .) }", pipe), linter)
  },
  .test_name = names(pipes),
  pipe = pipes
)

patrick::with_parameters_test_that(
  "unnecessary_placeholder_linter blocks simple disallowed usages",
  {
    expect_lint(
      sprintf("x %s sum(.)", pipe),
      rex::rex("Don't use the placeholder (`.`) when it's not needed"),
      unnecessary_placeholder_linter()
    )

    expect_lint(
      sprintf("x %s y(.) %s sum()", pipe, pipe),
      rex::rex("Don't use the placeholder (`.`) when it's not needed"),
      unnecessary_placeholder_linter()
    )
  },
  .test_name = names(pipes),
  pipe = pipes
)

test_that("lints vectorize", {
  lint_msg <- rex::rex("Don't use the placeholder (`.`) when it's not needed")

  expect_lint(
    trim_some("{
      x %>% foo(.)
      y %T>% bar(.)
    }"),
    list(
      list(lint_msg, line_number = 2L),
      list(lint_msg, line_number = 3L)
    ),
    unnecessary_placeholder_linter()
  )
})
# nofuzz end
