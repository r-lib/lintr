test_that("returns the correct linting", {
  expect_lint("fun(x, a = 1)",
    NULL,
    missing_argument_linter())

  expect_lint("fun(x = 1, a = 1)",
    NULL,
    missing_argument_linter())

  expect_lint("dt[, 1]",
    NULL,
    missing_argument_linter())

  expect_lint("dt[, 'col']",
    NULL,
    missing_argument_linter())

  expect_lint("array[, , 1]",
    NULL,
    missing_argument_linter())

  expect_lint("switch(a =, b =, c = 1, 0)",
    NULL,
    missing_argument_linter())

  expect_lint("alist(a =, b =, c = 1, 0)",
    NULL,
    missing_argument_linter())

  expect_lint("test(a =, b =, c = 1, 0)",
    NULL,
    missing_argument_linter("test"))

  expect_lint("fun(, a = 1)",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())

  expect_lint("f <- function(x, y) x\nf(, y = 1)\n",
    list(line = "f(, y = 1)"),
    missing_argument_linter())

  expect_lint("fun(a = 1,, b = 2)",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())

  expect_lint("fun(a = 1, b =)",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())

  expect_lint("fun(a = 1,)",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())

  expect_lint("fun(a = )",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())

  expect_lint("list(
    a = 1,
    b = 2,
    )",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())

  expect_lint("stats::median(1:10, na.rm =)",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())

  expect_lint("env$get(1:10, default =)",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())

  expect_lint("switch(a =, b = 1, 0)",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter(NULL))

  expect_lint("alist(a =)",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter(NULL))

  # Fixes https://github.com/r-lib/lintr/issues/906
  # Comments should be ignored so that missing arguments could be
  # properly identified in these cases.
  expect_lint("fun(
    1,
    2,
    # comment
    )",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())

  expect_lint("fun(
    # comment
    ,
    1
    )",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())

  expect_lint("fun(
    a = # comment
    ,
    1
    )",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter())
})
