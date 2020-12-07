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
    missing_argument_linter(c("test")))

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
    missing_argument_linter(c()))

  expect_lint("alist(a =)",
    list(message = rex("Missing argument in function call.")),
    missing_argument_linter(c()))
})
