test_that("missing_argument_linter skips allowed usages", {
  linter <- missing_argument_linter()

  expect_lint("fun(x, a = 1)", NULL, linter)
  expect_lint("fun(x = 1, a = 1)", NULL, linter)
  expect_lint("dt[, 1]", NULL, linter)
  expect_lint("dt[, 'col']", NULL, linter)
  expect_lint("array[, , 1]", NULL, linter)
  expect_lint("switch(a =, b =, c = 1, 0)", NULL, linter)
  expect_lint("alist(a =, b =, c = 1, 0)", NULL, linter)
  expect_lint("pairlist(path = quote(expr = ))", NULL, linter) #1889

  expect_lint("test(a =, b =, c = 1, 0)", NULL, missing_argument_linter("test"))
})

test_that("missing_argument_linter blocks disallowed usages", {
  linter <- missing_argument_linter()
  lint_msg <- rex::rex("Missing argument in function call.")

  expect_lint("fun(, a = 1)", list(message = lint_msg), linter)
  expect_lint("f <- function(x, y) x\nf(, y = 1)\n", list(line = "f(, y = 1)"), linter)
  expect_lint("fun(a = 1,, b = 2)", list(message = lint_msg), linter)
  expect_lint("fun(a = 1, b =)", list(message = lint_msg), linter)
  expect_lint("fun(a = 1,)", list(message = lint_msg), linter)
  expect_lint("fun(a = )", list(message = lint_msg), linter)

  expect_lint(
    trim_some("
      list(
        a = 1,
        b = 2,
      )
    "),
    list(message = lint_msg),
    linter
  )

  expect_lint("stats::median(1:10, na.rm =)", list(message = lint_msg), linter)
  expect_lint("env$get(1:10, default =)", list(message = lint_msg), linter)

  # except list can be empty
  expect_lint("switch(a =, b = 1, 0)", list(message = lint_msg), missing_argument_linter(character()))
  expect_lint("alist(a =)", list(message = lint_msg), missing_argument_linter(character()))

  # allow_trailing can allow trailing empty args also for non-excepted functions
  expect_lint("fun(a = 1,)", NULL, missing_argument_linter(allow_trailing = TRUE))
  expect_lint(
    trim_some("
      fun(
        a = 1,
        # comment
      )
    "),
    NULL,
    missing_argument_linter(allow_trailing = TRUE)
  )
  # ... but not if the final argument is named
  expect_lint(
    "fun(a = 1, b = )",
    list(message = lint_msg),
    missing_argument_linter(allow_trailing = TRUE)
  )

  # Fixes https://github.com/r-lib/lintr/issues/906
  # Comments should be ignored so that missing arguments could be
  # properly identified in these cases.
  expect_lint(
    trim_some("
      fun(
        1,
        2,
        # comment
      )
    "),
    list(message = lint_msg),
    linter
  )

  expect_lint(
    trim_some("
      fun(
        # comment
        ,
        1
      )
    "),
    list(message = lint_msg),
    linter
  )

  expect_lint(
    trim_some("
      fun(
        a = # comment
        ,
        1
      )
    "),
    list(message = lint_msg),
    linter
  )
})
