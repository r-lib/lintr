test_that("outer_negation_linter skips allowed usages", {
  linter <- outer_negation_linter()

  expect_no_lint("x <- any(y)", linter)
  expect_no_lint("y <- all(z)", linter)

  # extended usage of any is not covered
  expect_no_lint("any(!a & b)", linter)
  expect_no_lint("all(a | !b)", linter)

  expect_no_lint("any(a, b)", linter)
  expect_no_lint("all(b, c)", linter)
  expect_no_lint("any(!a, b)", linter)
  expect_no_lint("all(a, !b)", linter)
  expect_no_lint("any(a, !b, na.rm = TRUE)", linter)
  # ditto when na.rm is passed quoted
  expect_no_lint("any(a, !b, 'na.rm' = TRUE)", linter)
})

test_that("outer_negation_linter blocks simple disallowed usages", {
  linter <- outer_negation_linter()
  not_all_msg <- rex::rex("!all(x) is better than any(!x)")
  not_any_msg <- rex::rex("!any(x) is better than all(!x)")

  expect_lint("any(!x)", not_all_msg, linter)
  expect_lint("all(!foo(x))", not_any_msg, linter)
  # na.rm doesn't change the recommendation
  expect_lint("any(!x, na.rm = TRUE)", not_all_msg, linter)
  # also catch nested usage
  expect_lint("all(!(x + y))", not_any_msg, linter)
  # catch when all inputs are negated
  expect_lint("any(!x, !y)", not_all_msg, linter)
  expect_lint("all(!x, !y, na.rm = TRUE)", not_any_msg, linter)

  # adversarial comment
  expect_lint(
    trim_some("
      any(!x, na.rm = # comment
      TRUE)
    "),
    not_all_msg,
    linter
  )
})

test_that("outer_negation_linter doesn't trigger on empty calls", {
  linter <- outer_negation_linter()

  # minimal version of issue
  expect_no_lint("any()", linter)
  # closer to what was is practically relevant, as another regression test
  expect_no_lint("x %>% any()", linter)
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      any(!x)
      all(!y)
    }"),
    list(
      list(rex::rex("!all(x)"), line_number = 2L),
      list(rex::rex("!any(x)"), line_number = 3L)
    ),
    outer_negation_linter()
  )
})
