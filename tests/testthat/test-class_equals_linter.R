test_that("class_equals_linter skips allowed usages", {
  expect_lint("class(x) <- 'character'", NULL, class_equals_linter())
  expect_lint("class(x) = 'character'", NULL, class_equals_linter())

  # proper way to test exact class
  expect_lint("identical(class(x), c('glue', 'character'))", NULL, class_equals_linter())
})

test_that("class_equals_linter blocks simple disallowed usages", {
  expect_lint(
    "if (class(x) == 'character') stop('no')",
    rex::rex("Instead of comparing class(x) with =="),
    class_equals_linter()
  )

  expect_lint(
    "is_regression <- class(x) == 'lm'",
    rex::rex("Instead of comparing class(x) with =="),
    class_equals_linter()
  )

  expect_lint(
    "is_regression <- 'lm' == class(x)",
    rex::rex("Instead of comparing class(x) with =="),
    class_equals_linter()
  )
})

test_that("class_equals_linter blocks usage of %in% for checking class", {
  expect_lint(
    "if ('character' %in% class(x)) stop('no')",
    rex::rex("Instead of comparing class(x) with %in%"),
    class_equals_linter()
  )

  expect_lint(
    "if (class(x) %in% 'character') stop('no')",
    rex::rex("Instead of comparing class(x) with %in%"),
    class_equals_linter()
  )
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
  expect_lint("class(x)[class(x) == 'foo']", NULL, class_equals_linter())

  # but not further nesting
  expect_lint(
    "x[if (class(x) == 'foo') 1 else 2]",
    rex::rex("Instead of comparing class(x) with =="),
    class_equals_linter()
  )
})
