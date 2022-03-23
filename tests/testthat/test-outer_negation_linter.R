test_that("outer_negation_linter skips allowed usages", {
  expect_lint("x <- any(y)", NULL, outer_negation_linter())
  expect_lint("y <- all(z)", NULL, outer_negation_linter())

  # extended usage of any is not covered
  expect_lint("any(!a & b)", NULL, outer_negation_linter())
  expect_lint("all(a | !b)", NULL, outer_negation_linter())

  expect_lint("any(a, b)", NULL, outer_negation_linter())
  expect_lint("all(b, c)", NULL, outer_negation_linter())
  expect_lint("any(!a, b)", NULL, outer_negation_linter())
  expect_lint("all(a, !b)", NULL, outer_negation_linter())
  expect_lint("any(a, !b, na.rm = TRUE)", NULL, outer_negation_linter())
})

test_that("outer_negation_linter blocks simple disallowed usages", {
  expect_lint(
    "any(!x)",
    rex::rex("!all(x) is better than any(!x)"),
    outer_negation_linter()
  )

  expect_lint(
    "all(!foo(x))",
    rex::rex("!any(x) is better than all(!x)"),
    outer_negation_linter()
  )

  # na.rm doesn't change the recommendation
  expect_lint(
    "any(!x, na.rm = TRUE)",
    rex::rex("!all(x) is better than any(!x)"),
    outer_negation_linter()
  )

  # also catch nested usage
  expect_lint(
    "all(!(x + y))",
    rex::rex("!any(x) is better than all(!x)"),
    outer_negation_linter()
  )

  # catch when all inputs are negated
  expect_lint(
    "any(!x, !y)",
    rex::rex("!all(x) is better than any(!x)"),
    outer_negation_linter()
  )

  expect_lint(
    "all(!x, !y, na.rm = TRUE)",
    rex::rex("!any(x) is better than all(!x)"),
    outer_negation_linter()
  )
})

test_that("outer_negation_linter doesn't trigger on empty calls", {
  # minimal version of issue
  expect_lint("any()", NULL, outer_negation_linter())
  # closer to what was is practically relevant, as another regression test
  expect_lint("x %>% any()", NULL, outer_negation_linter())
})
