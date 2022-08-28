test_that("class_equals_linter skips allowed usages", {
  linter <- class_equals_linter()

  expect_lint("class(x) <- 'character'", NULL, linter)
  expect_lint("class(x) = 'character'", NULL, linter)

  # proper way to test exact class
  expect_lint("identical(class(x), c('glue', 'character'))", NULL, linter)
})

test_that("class_equals_linter blocks simple disallowed usages", {
  linter <- class_equals_linter()
  msg <- rex::rex("Instead of comparing class(x) with ==")

  expect_lint("if (class(x) == 'character') stop('no')", msg, linter)
  expect_lint("is_regression <- class(x) == 'lm'", msg, linter)
  expect_lint("is_regression <- 'lm' == class(x)", msg, linter)
})

test_that("class_equals_linter blocks usage of %in% for checking class", {
  linter <- class_equals_linter()
  msg <- rex::rex("Instead of comparing class(x) with %in%")

  expect_lint("if ('character' %in% class(x)) stop('no')", msg, linter)
  expect_lint("if (class(x) %in% 'character') stop('no')", msg, linter)
})

test_that("class_equals_linter blocks class(x) != 'klass'", {
  expect_lint(
    "if (class(x) != 'character') TRUE",
    rex::rex("Instead of comparing class(x) with !="),
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
    rex::rex("Instead of comparing class(x) with =="),
    linter
  )
})
