test_that("yoda_test_linter skips allowed usages", {
  expect_lint("expect_equal(x, 2)", NULL, yoda_test_linter())
  # namespace qualification doesn't matter
  expect_lint("testthat::expect_identical(x, 'a')", NULL, yoda_test_linter())
  # two variables can't be distinguished which is expected/actual (without
  #   playing quixotic games trying to parse that out from variable names)
  expect_lint("expect_equal(x, y)", NULL, yoda_test_linter())
})

test_that("yoda_test_linter blocks simple disallowed usages", {
  expect_lint(
    "expect_equal(2, x)",
    rex::rex("Tests should compare objects in the order 'actual', 'expected'"),
    yoda_test_linter()
  )
  expect_lint(
    "testthat::expect_identical('a', x)",
    rex::rex("Tests should compare objects in the order 'actual', 'expected'"),
    yoda_test_linter()
  )
  expect_lint(
    "expect_setequal(2, x)",
    rex::rex("Tests should compare objects in the order 'actual', 'expected'"),
    yoda_test_linter()
  )
  # complex literals are slightly odd
  expect_lint(
    "expect_equal(2 + 1i, x)",
    rex::rex("Tests should compare objects in the order 'actual', 'expected'"),
    yoda_test_linter()
  )
})

test_that("yoda_test_linter ignores strings in $ expressions", {
  # the "key" here shows up at the same level of the parse tree as plain "key" normally would
  expect_lint('expect_equal(x$"key", 2)', NULL, yoda_test_linter())
})

# if we only inspect the first argument & ignore context, get false positives
test_that("yoda_test_linter ignores usage in pipelines", {
  expect_lint("foo() %>% expect_identical(2)", NULL, yoda_test_linter())
  skip_if_not_r_version("4.1.0")
  expect_lint("bar() |> expect_equal('a')", NULL, yoda_test_linter())
})

test_that("yoda_test_linter throws a special message for placeholder tests", {
  expect_lint(
    "expect_equal(1, 1)",
    rex::rex("Avoid storing placeholder tests like expect_equal(1, 1)"),
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
