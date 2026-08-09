test_that("class_equals_linter skips allowed usages", {
  linter <- class_equals_linter()

  expect_no_lint("class(x) <- 'character'", linter)
  expect_no_lint("class(x) = 'character'", linter)

  # proper way to test exact class
  expect_no_lint("identical(class(x), c('glue', 'character'))", linter)
  expect_no_lint("is_lm <- inherits(x, 'lm')", linter)

  # allowed non-class is.element() usage
  expect_no_lint("is.element('foo', 'bar')", linter)

  # co-occurrence in separate arguments or comparisons
  expect_no_lint("foo(class(x), is.element(a, b))", linter)
})

test_that("class_equals_linter blocks simple disallowed usages", {
  linter <- class_equals_linter()
  lint_msg <- rex::rex("Use inherits(x, 'class-name'), is.<class> for S3 classes, or is(x, 'S4Class') for S4 classes")

  expect_lint("if (class(x) == 'character') stop('no')", lint_msg, linter)
  expect_lint("is_regression <- class(x) == 'lm'", lint_msg, linter)
  expect_lint("is_regression <- 'lm' == class(x)", lint_msg, linter)
  expect_lint(
    "class(x) == is.element(a, b)",
    rex::rex("instead of comparing class(x) with ==."),
    linter
  )
})

test_that("class_equals_linter blocks usage of %in% for checking class", {
  linter <- class_equals_linter()
  lint_msg <- rex::rex("Use inherits(x, 'class-name'), is.<class> for S3 classes, or is(x, 'S4Class') for S4 classes")

  expect_lint("if ('character' %in% class(x)) stop('no')", lint_msg, linter)
  expect_lint("if (class(x) %in% 'character') stop('no')", lint_msg, linter)
})

test_that("class_equals_linter blocks usage of is.element() for checking class", {
  linter <- class_equals_linter()
  lint_msg <- rex::rex("inherits(x, 'class-name')", one_or_more(any), "instead of comparing class(x) with is.element()")

  expect_lint("if (is.element('character', class(x))) stop('no')", lint_msg, linter)
  expect_lint("if (is.element(class(x), 'character')) stop('no')", lint_msg, linter)
  expect_lint("is.element(el = 'character', set = class(x))", lint_msg, linter)
  expect_lint("is.element(set = class(x), el = 'character')", lint_msg, linter)
  expect_lint("base::is.element('character', class(x))", lint_msg, linter)
  expect_lint("utils::is.element('character', class(x))", lint_msg, linter)

  # AST edge case
  expect_lint(
    trim_some("
      if (is.element #comment
      ('character', class(x))) TRUE
    "),
    lint_msg,
    linter
  )
})

test_that("class_equals_linter blocks class(x) != 'klass'", {
  expect_lint(
    "if (class(x) != 'character') TRUE",
    rex::rex("Use inherits(x, 'class-name'), is.<class> for S3 classes, or is(x, 'S4Class') for S4 classes"),
    class_equals_linter()
  )
})

# as seen, e.g. in base R
test_that("class_equals_linter skips usage for subsetting", {
  linter <- class_equals_linter()
  lint_message <- rex::rex("inherits(x, 'class-name'), is.<class> for S3 classes, or is(x, 'S4Class') for S4 classes")

  expect_no_lint("class(x)[class(x) == 'foo']", linter)
  expect_no_lint("class(x)[is.element('foo', class(x))]", linter)

  # but not further nesting
  expect_lint("x[if (class(x) == 'foo') 1 else 2]", lint_message, linter)
  expect_lint("x[if (is.element('foo', class(x))) 1 else 2]", lint_message, linter)
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      'character' %in% class(x)
      class(x) == 'character'
      is.element('character', class(x))
    }"),
    list(
      list("with %in%", line_number = 2L),
      list("with ==", line_number = 3L),
      list("with is\\.element", line_number = 4L)
    ),
    class_equals_linter()
  )
})
