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

  # always allow this missing usage
  expect_lint("foo()", NULL, linter)

  expect_lint("test(a =, b =, c = 1, 0)", NULL, missing_argument_linter("test"))
})

test_that("missing_argument_linter blocks disallowed usages", {
  linter <- missing_argument_linter()
  lint_msg1 <- rex::rex("Missing argument 1 in function call.")
  lint_msg2 <- rex::rex("Missing argument 2 in function call.")
  lint_msg3 <- rex::rex("Missing argument 3 in function call.")
  lint_msga <- rex::rex("Missing argument 'a' in function call.")

  expect_lint("fun(, a = 1)", lint_msg1, linter)
  expect_lint(
    "f <- function(x, y) x\nf(, y = 1)\n",
    list(lint_msg1, line = "f(, y = 1)"),
    linter
  )
  expect_lint("fun(a = 1,, b = 2)", lint_msg2, linter)
  expect_lint("fun(b = 1, a =)", lint_msga, linter)
  expect_lint("fun(a = 1,)", lint_msg2, linter)
  expect_lint("fun(a = )", lint_msga, linter)

  expect_lint(
    trim_some("
      list(
        a = 1,
        b = 2,
      )
    "),
    lint_msg3,
    linter
  )

  expect_lint("stats::median(1:10, a =)", lint_msga, linter)
  expect_lint("env$get(1:10, a =)", lint_msga, linter)

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
    lint_msg3,
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
    lint_msg1,
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
    lint_msga,
    linter
  )
})

test_that("except list can be empty", {
  linter <- missing_argument_linter(character())
  lint_msg <- rex::rex("Missing argument 'a' in function call.")

  expect_lint("switch(a =, b = 1, 0)", lint_msg, linter)
  expect_lint("alist(a =)", lint_msg, linter)
})

test_that("allow_trailing can allow trailing empty args also for non-excepted functions", {
  linter <- missing_argument_linter(allow_trailing = TRUE)

  expect_lint("fun(a = 1,)", NULL, linter)
  expect_lint(
    trim_some("
      fun(
        a = 1,
        # comment
      )
    "),
    NULL,
    linter
  )
  # ... but not if the final argument is named
  expect_lint(
    "fun(a = 1, b = )",
    rex::rex("Missing argument 'b' in function call."),
    linter
  )
})

test_that("lints vectorize", {
  linter <- missing_argument_linter()
  linter_trailing <- missing_argument_linter(allow_trailing = TRUE)
  lint_msg <- rex::rex("Missing argument in function call.")

  expect_lint(
    "foo(,,)",
    list(
      list("Missing argument 1", column_number = 5L),
      list("Missing argument 2", column_number = 6L),
      list("Missing argument 3", column_number = 7L)
    ),
    linter
  )
  expect_lint(
    "foo(,,)",
    list(
      list("Missing argument 1", column_number = 5L),
      list("Missing argument 2", column_number = 6L)
    ),
    linter_trailing
  )

  expect_lint(
    "foo(a =,,)",
    list(
      list("Missing argument 'a'", column_number = 7L),
      list("Missing argument 2", column_number = 9L),
      list("Missing argument 3", column_number = 10L)
    ),
    linter
  )
  expect_lint(
    "foo(a =,,)",
    list(
      list("Missing argument 'a'", column_number = 7L),
      list("Missing argument 2", column_number = 9L)
    ),
    linter_trailing
  )

  expect_lint(
    trim_some("{
      foo(1,)
      bar(,2)
    }"),
    list(
      list("Missing argument 2", line_number = 2L),
      list("Missing argument 1", line_number = 3L)
    ),
    linter
  )
  expect_lint(
    trim_some("{
      foo(1,)
      bar(,2)
    }"),
    list("Missing argument 1", line_number = 3L),
    linter_trailing
  )
})
