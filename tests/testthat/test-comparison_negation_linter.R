test_that("comparison_negation_linter skips allowed usages", {
  linter <- comparison_negation_linter()

  # doesn't apply to joint statements
  expect_no_lint("!(x == y | y == z)", linter)
  # don't force de Morgan's laws
  expect_no_lint("!(x & y)", linter)

  # naive xpath will include !foo(x) cases
  expect_no_lint("!any(x > y)", linter)
  # ditto for tidyeval cases
  expect_no_lint("!!target == 1 ~ 'target'", linter)
  # ditto for !x[f == g]
  expect_no_lint("!passes.test[stage == 1]", linter)
})

local({
  linter <- comparison_negation_linter()

  comparators <- c("==", "!=", ">=", ">", "<=", "<")
  inverses <- c("!=", "==", "<", "<=", ">", ">=")
  patrick::with_parameters_test_that(
    "comparison_negation_linter blocks simple disallowed usages",
    {
      expect_lint(
        sprintf("!(x %s y)", comparator),
        rex::rex(sprintf("Use x %s y, not !(x %s y)", inverse, comparator)),
        linter
      )
      expect_lint(
        sprintf("!x %s y", comparator),
        rex::rex(sprintf("Use x %s y, not !(x %s y)", inverse, comparator)),
        linter
      )
    },
    .test_name = comparators,
    comparator = comparators,
    inverse = inverses
  )
})

test_that("comparison_negation_linter catches plain ! (no parens) + call", {
  # Earlier logic would find the wrong node due to expr in length()
  expect_lint(
    "!length(x) > 0",
    rex::rex("Use x <= y, not !(x > y)"),
    comparison_negation_linter()
  )
})

test_that("Lints vectorize", {
  expect_lint(
    trim_some("{
      !(x > y)
      !x == y
    }"),
    list(
      list(rex::rex("Use x <= y, not !(x > y)."), line_number = 2L),
      list(rex::rex("Use x != y, not !(x == y)."), line_number = 3L)
    ),
    comparison_negation_linter()
  )
})

test_that("logic survives adversarial comments", {
  expect_lint(
    trim_some("
      !(x #
      > y)
    "),
    rex::rex("Use x <= y, not !(x > y)"),
    comparison_negation_linter()
  )
})
