test_that("returns the correct linting", {
  linter <- sprintf_linter()

  expect_lint(
    "sprintf('hello')",
    NULL,
    linter
  )

  expect_lint(
    "sprintf('hello', 1)",
    if (getRversion() >= "4.1.0") "one argument not used by format" else NULL,
    linter
  )

  expect_lint(
    "sprintf('hello %d', 1)",
    NULL,
    linter
  )

  expect_lint(
    "sprintf('hello %d', x)",
    NULL,
    linter
  )

  expect_lint(
    "sprintf('hello %d', x + 1)",
    NULL,
    linter
  )

  expect_lint(
    "sprintf('hello %d', f(x))",
    NULL,
    linter
  )

  expect_lint(
    "sprintf('hello %d', 'a')",
    list(message = rex("invalid format '%d'; use format %s for character objects")),
    linter
  )

  expect_lint(
    "sprintf('hello %d', 1.5)",
    list(message = rex("invalid format '%d'; use format %f, %e, %g or %a for numeric objects")),
    linter
  )

  expect_lint(
    "sprintf('hello %d',)",
    list(message = rex("argument is missing, with no default")),
    linter
  )

  expect_lint(
    "sprintf('hello %1$s %1$s', x)",
    NULL,
    linter
  )

  expect_lint(
    "sprintf('hello %1$s %s', 'a', 'b')",
    if (getRversion() >= "4.1.0") "one argument not used by format" else NULL,
    linter
  )

  expect_lint(
    "sprintf('hello %1$s %1$s', x, y)",
    if (getRversion() >= "4.1.0") "one argument not used by format" else NULL,
    linter
  )

  expect_lint(
    "sprintf('hello %1$s %1$s %2$d', x, y)",
    NULL,
    linter
  )

  expect_lint(
    "sprintf('hello %1$s %1$s %3$d', x, y)",
    list(message = rex("reference to non-existent argument 3")),
    linter
  )

  expect_lint(
    "sprintf('hello %1$s %1$s %2$d %3$d', x, y, 1.5)",
    list(message = rex("invalid format '%d'; use format %f, %e, %g or %a for numeric objects")),
    linter
  )

  expect_lint(
    "sprintf('hello %1$s %1$s %2$d %3$s', x, y, 1.5)",
    NULL,
    linter
  )

  # works with multi-line sprintf and comments
  expect_lint(
    trim_some("
      sprintf(
        'test fmt %s', # this is a comment
        2
      )
    "),
    NULL,
    linter
  )
})

test_that("edge cases are detected correctly", {
  linter <- sprintf_linter()

  # dots
  expect_lint(
    "sprintf('%d %d, %d', id, ...)",
    NULL,
    linter
  )

  # TODO (@AshesITR) extend ... detection to at least test for too many arguments.

  # named argument fmt
  expect_lint(
    "sprintf(x, fmt = 'hello %1$s %1$s')",
    NULL,
    linter
  )

  expect_lint(
    "sprintf(x, fmt = 'hello %1$s %1$s %3$d', y)",
    list(message = rex("reference to non-existent argument 3")),
    linter
  )
})
