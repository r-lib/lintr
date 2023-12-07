test_that("yoda_test_linter skips allowed usages", {
  linter <- yoda_test_linter()

  expect_lint("expect_equal(x, 2)", NULL, linter)
  # namespace qualification doesn't matter
  expect_lint("testthat::expect_identical(x, 'a')", NULL, linter)
  # two variables can't be distinguished which is expected/actual (without
  #   playing quixotic games trying to parse that out from variable names)
  expect_lint("expect_equal(x, y)", NULL, linter)
})

test_that("yoda_test_linter blocks simple disallowed usages", {
  linter <- yoda_test_linter()
  lint_msg <- rex::rex("Compare objects in tests in the order 'actual', 'expected', not the reverse.")

  expect_lint("expect_equal(2, x)", lint_msg, linter)
  expect_lint("testthat::expect_identical('a', x)", lint_msg, linter)
  expect_lint("expect_setequal(2, x)", lint_msg, linter)
  # complex literals are slightly odd
  expect_lint("expect_equal(2 + 1i, x)", lint_msg, linter)
})

test_that("yoda_test_linter ignores strings in $ expressions", {
  linter <- yoda_test_linter()

  # the "key" here shows up at the same level of the parse tree as plain "key" normally would
  expect_lint('expect_equal(x$"key", 2)', NULL, linter)
  expect_lint('expect_equal(x@"key", 2)', NULL, linter)
})

# if we only inspect the first argument & ignore context, get false positives
local({
  pipes <- pipes(exclude = c("%<>%", "%$%"))
  linter <- yoda_test_linter()
  patrick::with_parameters_test_that(
    "yoda_test_linter ignores usage in pipelines",
    expect_lint(sprintf("foo() %s expect_identical(2)", pipe), NULL, linter),
    pipe = pipes,
    .test_name = names(pipes)
  )
})

test_that("yoda_test_linter throws a special message for placeholder tests", {
  expect_lint(
    "expect_equal(1, 1)",
    rex::rex("Avoid storing placeholder tests like expect_equal(1, 1)"),
    yoda_test_linter()
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      expect_equal(1, 1)
      expect_equal(2, foo(x))
    }"),
    list(
      list("Avoid storing placeholder tests", line_number = 2L),
      list("Compare objects in tests in the order 'actual', 'expected'", line_number = 3L)
    ),
    yoda_test_linter()
  )
})

# TODO(michaelchirico): Should this be extended to RUnit tests? It seems yes,
#   but the argument names in RUnit (inherited from base all.equal()) are a bit
#   confusing, e.g. `checkEqual(target=, current=)`. From the name, one might
#   reasonably conclude 'expected' comes first, and 'actual' comes second.
# TODO(michaelchirico): What sorts of combinations of literals can be included?
#   e.g. expect_equal(c(1, 2), x) is a yoda test; is expect_equal(c(x, 1), y)?
#   clearly it's not true for general f() besides c(). What about other
#   constructors of literals? data.frame(), data.table(), tibble(), ...?
# TODO(michaelchirico): The logic could also be extended to "tests" inside regular
#   code, not just test suites, e.g. `if (2 == x)`, `while(3 <= x)`,
#   `stopifnot('a' == foo(y))`.
