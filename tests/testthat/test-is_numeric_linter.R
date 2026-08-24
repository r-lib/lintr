test_that("is_numeric_linter skips allowed usages involving ||", {
  linter <- is_numeric_linter()

  expect_no_lint("is.numeric(x) || is.integer(y)", linter)
  # x is used, but not identically
  expect_no_lint("is.numeric(x) || is.integer(foo(x))", linter)
  # not totally crazy, e.g. if input accepts a vector or a list
  expect_no_lint("is.numeric(x) || is.integer(x[[1]])", linter)

  # closure boundaries
  expect_no_lint("(\\() is.numeric(x)) || is.integer(x)", linter)
  expect_no_lint("(function() is.numeric(x)) || is.integer(x)", linter)

  # multi-term || chains with differing symbols or non-redundant types
  expect_no_lint("is.numeric(x) || is.integer(y) || is.factor(z)", linter)
  expect_no_lint("is.numeric(x) || is.factor(x) || is.character(x)", linter)

  # && subexpressions inside || chains
  expect_no_lint("(is.numeric(x) && foo(x)) || is.integer(x)", linter)
})

test_that("is_numeric_linter skips allowed usages involving %in%", {
  linter <- is_numeric_linter()

  # false positives for class(x) %in% c('integer', 'numeric') style
  expect_no_lint("class(x) %in% 1:10", linter)
  expect_no_lint("class(x) %in% 'numeric'", linter)
  expect_no_lint("class(x) %in% c('numeric', 'integer', 'factor')", linter)
  expect_no_lint("class(x) %in% c('numeric', 'integer', y)", linter)
  expect_no_lint(
    trim_some("{
      class(a) %in% c('integer', 'factor')
      class(b) %in% c('logical', 'numeric')
    }"),
    linter
  )
})

test_that("is_numeric_linter blocks disallowed usages involving ||", {
  linter <- is_numeric_linter()
  lint_msg <- rex::rex("Use `is.numeric(x)` instead of the equivalent `is.numeric(x) || is.integer(x)`.")

  expect_lint("is.numeric(x) || is.integer(x)", lint_msg, linter)

  # order doesn't matter
  expect_lint("is.integer(x) || is.numeric(x)", lint_msg, linter)

  # identical expressions match too
  expect_lint( # nofuzz: dollar_at
    "is.integer(DT$x) || is.numeric(DT$x)",
    lint_msg,
    linter
  )
  expect_lint("is.numeric(foo(x)) || is.integer(foo(x))", lint_msg, linter)

  # named arguments and namespaces
  expect_lint("is.numeric(x = a) || is.integer(a)", lint_msg, linter)
  expect_lint("is.numeric(a) || is.integer(x = a)", lint_msg, linter)
  expect_lint("base::is.numeric(x) || base::is.integer(x)", lint_msg, linter)

  # line breaks and comments don't matter
  lines <- trim_some("
    if (
      is.integer(x)
      || is.numeric(x)
    ) TRUE
  ")
  expect_lint(lines, lint_msg, linter)
  expect_lint(
    trim_some("
      is.numeric( # comment
        foo(x)
      ) || is.integer( # comment
        foo( # comment
          x # comment
        ) # comment
      )
    "),
    lint_msg,
    linter
  )

  # caught when nesting
  expect_lint("all(y > 5) && (is.integer(x) || is.numeric(x))", lint_msg, linter)

  # implicit nesting and mixed order in || chains (#1636)
  expect_lint("is.integer(x) || is.numeric(x) || is.logical(x)", lint_msg, linter)
  expect_lint("is.logical(x) || is.integer(x) || is.numeric(x)", lint_msg, linter)
  expect_lint("is.factor(x) || is.numeric(x) || is.integer(x)", lint_msg, linter)
  expect_lint("is.numeric(x) || is.logical(x) || is.integer(x)", lint_msg, linter)
  expect_lint("is.factor(x) || is.numeric(x) || is.logical(x) || is.integer(x)", lint_msg, linter)
  expect_lint("is.factor(x) || (is.numeric(x) || is.integer(x))", lint_msg, linter)
  expect_lint("((is.factor(x) || is.numeric(x))) || is.integer(x)", lint_msg, linter)
  expect_lint("foo() || is.numeric(x) || is.integer(x)", lint_msg, linter)
  expect_lint("is.factor(x) || base::is.numeric(x) || is.integer(x = x)", lint_msg, linter)
  expect_lint(
    "is.numeric(x) || is.integer(x) || is.numeric(y) || is.integer(y)",
    list(
      list(lint_msg, column_number = 1L),
      list(lint_msg, column_number = 1L)
    ),
    linter
  )
  expect_lint(
    "(is.numeric(x) || is.integer(x)) || (is.numeric(y) || is.integer(y))",
    list(
      list(lint_msg, column_number = 2L),
      list(lint_msg, column_number = 38L)
    ),
    linter
  )
})

test_that("is_numeric_linter blocks disallowed usages involving %in%", {
  linter <- is_numeric_linter()
  lint_msg <- rex::rex('Use is.numeric(x) instead of class(x) %in% c("integer", "numeric")')

  expect_lint("class(x) %in% c('integer', 'numeric')", lint_msg, linter)
  expect_lint('class(x) %in% c("numeric", "integer")', lint_msg, linter)
})

test_that("raw strings are handled properly when testing in class", {
  linter <- is_numeric_linter()
  lint_msg <- rex::rex('Use is.numeric(x) instead of class(x) %in% c("integer", "numeric")')

  expect_no_lint("class(x) %in% c(R'(numeric)', 'integer', 'factor')", linter)
  expect_no_lint("class(x) %in% c('numeric', R'--(integer)--', y)", linter)

  expect_lint("class(x) %in% c(R'(integer)', 'numeric')", lint_msg, linter)
  expect_lint('class(x) %in% c("numeric", R"--[integer]--")', lint_msg, linter)
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      is.numeric(x) || is.integer(x)
      class(x) %in% c('integer', 'numeric')
      is.logical(x) || is.integer(x) || is.numeric(x)
    }"),
    list(
      list(rex::rex("`is.numeric(x) || is.integer(x)`"), line_number = 2L),
      list(rex::rex('class(x) %in% c("integer", "numeric")'), line_number = 3L),
      list(rex::rex("`is.numeric(x) || is.integer(x)`"), line_number = 4L)
    ),
    is_numeric_linter()
  )
})
