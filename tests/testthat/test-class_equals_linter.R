test_that("class_equals_linter skips allowed usages", {
  linter <- class_equals_linter()

  expect_lint("class(x) <- 'character'", NULL, linter)
  expect_lint("class(x) = 'character'", NULL, linter)

  # proper way to test exact class
  expect_lint("identical(class(x), c('glue', 'character'))", NULL, linter)
  expect_lint("is_lm <- inherits(x, 'lm')", NULL, linter)
})

test_that("class_equals_linter blocks simple disallowed usages", {
  linter <- class_equals_linter()
  lint_msg <- rex::rex("Use inherits(x, 'class-name'), is.<class> or is(x, 'class')")

  expect_lint("if (class(x) == 'character') stop('no')", lint_msg, linter)
  expect_lint("is_regression <- class(x) == 'lm'", lint_msg, linter)
  expect_lint("is_regression <- 'lm' == class(x)", lint_msg, linter)
})

test_that("class_equals_linter blocks usage of %in% for checking class", {
  linter <- class_equals_linter()
  lint_msg <- rex::rex("Use inherits(x, 'class-name'), is.<class> or is(x, 'class')")

  expect_lint("if ('character' %in% class(x)) stop('no')", lint_msg, linter)
  expect_lint("if (class(x) %in% 'character') stop('no')", lint_msg, linter)
})

test_that("class_equals_linter blocks class(x) != 'klass'", {
  expect_lint(
    "if (class(x) != 'character') TRUE",
    rex::rex("Use inherits(x, 'class-name'), is.<class> or is(x, 'class')"),
    class_equals_linter()
  )
})

# as seen, e.g. in base R
test_that("class_equals_linter skips usage for subsetting", {
  linter <- class_equals_linter()

  expect_lint("class(x)[class(x) == 'foo']", NULL, linter)

  # but not further nesting
  expect_lint(
    "x[if (class(x) == 'foo') 1 else 2]",
    rex::rex("Use inherits(x, 'class-name'), is.<class> or is(x, 'class')"),
    linter
  )
})
