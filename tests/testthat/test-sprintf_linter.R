context("sprintf_linter")
test_that("returns the correct linting", {
  expect_lint("sprintf('hello')",
    NULL,
    sprintf_linter)

  expect_lint("sprintf('hello', 1)",
    NULL,
    sprintf_linter)

  expect_lint("sprintf('hello %d', 1)",
    NULL,
    sprintf_linter)

  expect_lint("sprintf('hello %d', x)",
    NULL,
    sprintf_linter)

  expect_lint("sprintf('hello %d', x + 1)",
    NULL,
    sprintf_linter)

  expect_lint("sprintf('hello %d', f(x))",
    NULL,
    sprintf_linter)

  expect_lint("sprintf('hello %d', 'a')",
    list(message = rex("invalid format '%d'; use format %s for character objects")),
    sprintf_linter)

  expect_lint("sprintf('hello %d', 1.5)",
    list(message = rex("invalid format '%d'; use format %f, %e, %g or %a for numeric objects")),
    sprintf_linter)

  expect_lint("sprintf('hello %d',)",
    list(message = rex("argument is missing, with no default")),
    sprintf_linter)

  expect_lint("sprintf('hello %1$s %1$s', x)",
    NULL,
    sprintf_linter)

  expect_lint("sprintf('hello %1$s %1$s', x, y)",
    NULL,
    sprintf_linter)

  expect_lint("sprintf('hello %1$s %1$s %2$d', x, y)",
    NULL,
    sprintf_linter)

  expect_lint("sprintf('hello %1$s %1$s %3$d', x, y)",
    list(message = rex("reference to non-existent argument 3")),
    sprintf_linter)

  expect_lint("sprintf('hello %1$s %1$s %2$d %3$d', x, y, 1.5)",
    list(message = rex("invalid format '%d'; use format %f, %e, %g or %a for numeric objects")),
    sprintf_linter)

  expect_lint("sprintf('hello %1$s %1$s %2$d %3$s', x, y, 1.5)",
    NULL,
    sprintf_linter)
})
