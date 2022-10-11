test_that("unnecessary_placeholder_linter skips allowed usages", {
  # . used in position other than first --> ok
  expect_lint("x %>% foo(y, .)", NULL, unnecessary_placeholder_linter())
  # ditto for nested usage
  expect_lint("x %>% foo(y, bar(.))", NULL, unnecessary_placeholder_linter())
  # . used twice --> ok
  expect_lint("x %>% foo(., .)", NULL, unnecessary_placeholder_linter())
  # . used as a keyword argument --> ok
  expect_lint("x %>% foo(arg = .)", NULL, unnecessary_placeholder_linter())
})

test_that("unnecessary_placeholder_linter blocks simple disallowed usages", {
  expect_lint(
    "x %>% sum(.)",
    rex::rex("Don't use the placeholder (`.`) when it's not needed"),
    unnecessary_placeholder_linter()
  )

  # can come anywhere in the pipeline
  expect_lint(
    "x %>% y(.) %>% sum()",
    rex::rex("Don't use the placeholder (`.`) when it's not needed"),
    unnecessary_placeholder_linter()
  )
})
