test_that("unnecessary_lambda_linter skips allowed usages", {
  linter <- unnecessary_lambda_linter()
  expect_lint("lapply(DF, sum)", NULL, linter)
  expect_lint("apply(M, 1, sum, na.rm = TRUE)", NULL, linter)

  # the first argument may be ... or have a cumbersome name, so an anonymous
  #   function may be preferable (e.g. this is often the case for grep() calls)
  expect_lint("sapply(x, function(xi) foo(1, xi))", NULL, linter)
  expect_lint("sapply(x, function(xi) return(foo(1, xi)))", NULL, linter)

  # if the argument is re-used, that's also a no-go
  expect_lint("dendrapply(x, function(xi) foo(xi, xi))", NULL, linter)
  # at any nesting level
  expect_lint("parLapply(cl, x, function(xi) foo(xi, 2, bar(baz(xi))))", NULL, linter)

  # multi-expression case
  expect_lint("lapply(x, function(xi) { print(xi); xi^2 })", NULL, linter)
  # multi-expression, multi-line cases
  expect_lint(
    trim_some("
      lapply(x, function(xi) {
        print(xi); xi^2
      })
    "),
    NULL,
    linter
  )
  expect_lint(
    trim_some("
      lapply(x, function(xi) {
        print(xi)
        xi^2
      })
    "),
    NULL,
    linter
  )

  # This _could_ be lapply(x, `%in%`, tbl), but don't force infix into lambda
  expect_lint("lapply(x, function(xi) xi %in% tbl)", NULL, linter)
  # This one could not
  expect_lint("lapply(x, function(xi) tbl %in% xi)", NULL, linter)
  # would require multiple lapply() loops
  expect_lint("lapply(x, function(xi) foo(bar(xi)))", NULL, linter)
  expect_lint("lapply(x, function(xi) return(foo(bar(xi))))", NULL, linter)

  # extractions, #2231
  expect_lint("lapply(l, function(x) rle(x)$values)", NULL, linter)
  expect_lint('lapply(l, function(x) rle(x)["values"])', NULL, linter)
  expect_lint('lapply(l, function(x) rle(x)[["values"]])', NULL, linter)
  expect_lint("lapply(l, function(x) rle(x)@values)", NULL, linter)

  # return() extractions, #2258
  expect_lint("lapply(l, function(x) return(foo(x)$bar))", NULL, linter)
  expect_lint('lapply(l, function(x) return(rle(x)["values"]))', NULL, linter)
  expect_lint('lapply(l, function(x) return(rle(x)[["values"]]))', NULL, linter)
  expect_lint("lapply(l, function(x) return(rle(x)@values))", NULL, linter)

  # Other operators, #2247
  expect_lint("lapply(l, function(x) foo(x) - 1)", NULL, linter)
  expect_lint("lapply(l, function(x) foo(x) * 2)", NULL, linter)
  expect_lint("lapply(l, function(x) foo(x) ^ 3)", NULL, linter)
  expect_lint("lapply(l, function(x) foo(x) %% 4)", NULL, linter)

  # Don't include other lambdas, #2249
  expect_lint(
    trim_some('{
      lapply(x, function(e) sprintf("%o", e))
      lapply(y, function(e) paste(strlpad(e, "0", width)))
    }'),
    NULL,
    linter
  )

  # only call is on RHS of operator, #2310
  expect_lint("lapply(l, function(x) 'a' %in% names(x))", NULL, linter)
  expect_lint("lapply(l, function(x = 1) 'a' %in% names(x))", NULL, linter)
})

test_that("unnecessary_lambda_linter skips allowed inner comparisons", {
  linter <- unnecessary_lambda_linter()

  # lapply returns a list, so not the same, though as.list is probably
  #   a better choice
  expect_lint("lapply(x, function(xi) foo(xi) == 2)", NULL, linter)

  # this _may_ return a matrix, though outer is probably a better choice if so
  expect_lint("sapply(x, function(xi) foo(xi) == y)", NULL, linter)

  # only lint "plain" calls that can be replaced by eliminating the lambda
  expect_lint("sapply(x, function(xi) sum(abs(xi)) == 0)", NULL, linter)
})

test_that("unnecessary_lambda_linter blocks simple disallowed usage", {
  linter <- unnecessary_lambda_linter()

  expect_lint(
    "lapply(DF, function(x) sum(x))",
    rex::rex("Pass sum directly as a symbol to lapply()"),
    linter
  )
  expect_lint(
    "lapply(DF, function(x) return(sum(x)))",
    rex::rex("Pass sum directly as a symbol to lapply()"),
    linter
  )

  expect_lint(
    "rapply(l, function(x) is.data.frame(x))",
    rex::rex("Pass is.data.frame directly as a symbol to rapply()"),
    linter
  )

  expect_lint(
    "eapply(env, function(x) sum(x, na.rm = TRUE))",
    rex::rex("Pass sum directly as a symbol to eapply()"),
    linter
  )
  expect_lint(
    "eapply(env, function(x) return(sum(x, na.rm = TRUE)))",
    rex::rex("Pass sum directly as a symbol to eapply()"),
    linter
  )
})

test_that("unnecessary_lambda_linter blocks simple disallowed usages", {
  linter <- unnecessary_lambda_linter()
  linter_allow <- unnecessary_lambda_linter(allow_comparison = TRUE)
  lint_msg <- rex::rex("Compare to a constant after calling sapply() to get", anything, "sapply(x, foo)")

  expect_lint("sapply(x, function(xi) foo(xi) == 2)", lint_msg, linter)
  expect_lint("sapply(x, function(xi) foo(xi) == 'a')", lint_msg, linter)
  expect_lint("sapply(x, function(xi) foo(xi) == 1 + 2i)", lint_msg, linter)

  expect_lint("sapply(x, function(xi) foo(xi) == 2)", NULL, linter_allow)
  expect_lint("sapply(x, function(xi) foo(xi) == 'a')", NULL, linter_allow)
  expect_lint("sapply(x, function(xi) foo(xi) == 1 + 2i)", NULL, linter_allow)

  # vapply counts as well
  # NB: we ignore the FUN.VALUE argument, for now
  expect_lint(
    "vapply(x, function(xi) foo(xi) == 2, logical(1L))",
    rex::rex("Compare to a constant after calling vapply()", anything, "vapply(x, foo, FUN.VALUE = <intermediate>)"),
    linter
  )
})

test_that("unnecessary_lambda_linter blocks other comparators as well", {
  linter <- unnecessary_lambda_linter()
  linter_allow <- unnecessary_lambda_linter(allow_comparison = TRUE)
  lint_msg <- rex::rex("Compare to a constant after calling sapply() to get")

  expect_lint("sapply(x, function(xi) foo(xi) >= 2)", lint_msg, linter)
  expect_lint("sapply(x, function(xi) foo(xi) != 'a')", lint_msg, linter)
  expect_lint("sapply(x, function(xi) foo(xi) < 1 + 2i)", lint_msg, linter)

  expect_lint("sapply(x, function(xi) foo(xi) >= 2)", NULL, linter_allow)
  expect_lint("sapply(x, function(xi) foo(xi) != 'a')", NULL, linter_allow)
  expect_lint("sapply(x, function(xi) foo(xi) < 1 + 2i)", NULL, linter_allow)
})

test_that("unnecessary_lambda_linter doesn't apply to keyword args", {
  expect_lint("lapply(x, function(xi) data.frame(nm = xi))", NULL, unnecessary_lambda_linter())
  expect_lint("lapply(x, function(xi) return(data.frame(nm = xi)))", NULL, unnecessary_lambda_linter())
})

test_that("purrr-style anonymous functions are also caught", {
  linter <- unnecessary_lambda_linter()

  expect_lint("purrr::map(x, ~.x)", NULL, linter)
  expect_lint("purrr::map_df(x, ~lm(y, .x))", NULL, linter)
  expect_lint("map_dbl(x, ~foo(bar = .x))", NULL, linter)

  expect_lint(
    "purrr::map(x, ~foo(.x))",
    rex::rex("Pass foo directly as a symbol to map()"),
    linter
  )
  expect_lint(
    "purrr::map_int(x, ~foo(.x, y))",
    rex::rex("Pass foo directly as a symbol to map_int()"),
    linter
  )
  expect_lint(
    "purrr::map_vec(x, ~foo(.x, y))",
    rex::rex("Pass foo directly as a symbol to map_vec()"),
    linter
  )
})

test_that("cases with braces are caught", {
  linter <- unnecessary_lambda_linter()
  lint_msg <- rex::rex("Pass print directly as a symbol to lapply()")

  expect_lint(
    "lapply(x, function(xi) { print(xi) })",
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      lapply(x, function(xi) {
        print(xi)
      })
    "),
    lint_msg,
    linter
  )

  expect_lint(
    "lapply(x, function(xi) { return(print(xi)) })",
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      lapply(x, function(xi) {
        print(xi)
      })
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      lapply(x, function(xi) {
        return(print(xi))
      })
    "),
    lint_msg,
    linter
  )

  expect_lint(
    trim_some("
      lapply(x, function(xi) {
        print(xi)
        xi
      })
    "),
    NULL,
    linter
  )

  # false positives like #2231, #2247 are avoided with braces too
  expect_lint("lapply(x, function(xi) { foo(xi)$bar })", NULL, linter)
  expect_lint("lapply(x, function(xi) { foo(xi) - 1 })", NULL, linter)
})

test_that("function shorthand is handled", {
  skip_if_not_r_version("4.1.0")

  expect_lint(
    "lapply(DF, \\(x) sum(x))",
    rex::rex("Pass sum directly as a symbol to lapply()"),
    unnecessary_lambda_linter()
  )
})

test_that("lints vectorize", {
  expect_lint(
    trim_some("{
      sapply(x, function(xi) sd(xi))
      lapply(y, function(yi) {
        sum(yi)
      })
    }"),
    list(
      list("sd", line_number = 2L),
      list("sum", line_number = 3L)
    ),
    unnecessary_lambda_linter()
  )
})
