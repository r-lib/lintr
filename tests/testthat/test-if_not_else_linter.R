test_that("if_not_else_linter skips allowed usages", {
  linter <- if_not_else_linter()

  # simple if/else statement is fine
  expect_lint("if (A) x else y", NULL, linter)
  # not plain negation --> OK
  expect_lint("if (!A || B) x else y", NULL, linter)
  # no else clause --> OK
  expect_lint("if (!A) x", NULL, linter)

  # nested statements are also OK
  expect_lint("if (!A) x else if (B) y", NULL, linter)
  expect_lint("if (!A) x else if (B) y else z", NULL, linter)
  expect_lint("if (A) x else if (B) y else if (!C) z", NULL, linter)

  # ! picked up in the evaluation statements is skipped
  expect_lint("if (A) !x else y", NULL, linter)
  expect_lint("if (A) x else !y", NULL, linter)
})

test_that("if_not_else_linter blocks simple disallowed usages", {
  linter <- if_not_else_linter()
  lint_msg <- rex::rex("Prefer `if (A) x else y`")

  expect_lint("if (!A) x else y", lint_msg, linter)
  expect_lint("if (!A) x else if (!B) y else z", lint_msg, linter)

  # ditto for more complicated expressions where ! is still the outer operator
  expect_lint("if (!x %in% 1:10) y else z", lint_msg, linter)
})

patrick::with_parameters_test_that(
  "if_not_else_linter blocks usages in ifelse() and friends as well",
  {
    linter <- if_not_else_linter()
    expect_lint(sprintf("%s(!A | B, x, y)", ifelse_fun), NULL, linter)
    expect_lint(sprintf("%s(A, !x, y)", ifelse_fun), NULL, linter)
    expect_lint(
      sprintf("%s(!A, x, y)", ifelse_fun),
      sprintf("Prefer `%s[(]A, x, y[)]` to the less-readable", ifelse_fun),
      linter
    )
    # particularly relevant for if_else()
    expect_lint(sprintf("%s(!!A, x, y)", ifelse_fun), NULL, linter)
  },
  .test_name = c("ifelse", "fifelse", "if_else"),
  ifelse_fun = c("ifelse", "fifelse", "if_else")
)

test_that("if_not_else_linter skips negated calls to is.null & similar", {
  linter <- if_not_else_linter()

  expect_lint("if (!is.null(x)) x else y", NULL, linter)
  expect_lint("if (!is.na(x)) x else y", NULL, linter)
  expect_lint("if (!missing(x)) x else y", NULL, linter)
  expect_lint("ifelse(!is.na(x), x, y)", NULL, linter)
})

test_that("multiple lints are generated correctly", {
  expect_lint(
    trim_some("{
        if (!A) x else B
        ifelse(!A, x, y)
        fifelse(!A, x, y)
        if_else(!A, x, y)
    }"),
    list(
      rex::rex("Prefer `if (A) x else y`"),
      "Prefer `ifelse",
      "Prefer `fifelse",
      "Prefer `if_else"
    ),
    if_not_else_linter()
  )
})

test_that("exceptions= argument works", {
  expect_lint(
    "if (!is.null(x)) x else y",
    rex::rex("Prefer `if (A) x else y`"),
    if_not_else_linter(exceptions = character())
  )

  expect_lint("if (!foo(x)) y else z", NULL, if_not_else_linter(exceptions = "foo"))
})
