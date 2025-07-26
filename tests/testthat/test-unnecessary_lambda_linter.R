test_that("unnecessary_lambda_linter skips allowed usages", {
  linter <- unnecessary_lambda_linter()
  expect_no_lint("lapply(DF, sum)", linter)
  expect_no_lint("apply(M, 1, sum, na.rm = TRUE)", linter)

  # the first argument may be ... or have a cumbersome name, so an anonymous
  #   function may be preferable (e.g. this is often the case for grep() calls)
  expect_no_lint("sapply(x, function(xi) foo(1, xi))", linter)
  expect_no_lint("sapply(x, function(xi) return(foo(1, xi)))", linter)

  # if the argument is re-used, that's also a no-go
  expect_no_lint("dendrapply(x, function(xi) foo(xi, xi))", linter)
  # at any nesting level
  expect_no_lint("parLapply(cl, x, function(xi) foo(xi, 2, bar(baz(xi))))", linter)

  # multi-expression case
  expect_no_lint("lapply(x, function(xi) { print(xi); xi^2 })", linter)
  # multi-expression, multi-line cases
  expect_no_lint(
    trim_some("
      lapply(x, function(xi) {
        print(xi); xi^2
      })
    "),
    linter
  )
  expect_no_lint(
    trim_some("
      lapply(x, function(xi) {
        print(xi)
        xi^2
      })
    "),
    linter
  )

  # This _could_ be lapply(x, `%in%`, tbl), but don't force infix into lambda
  expect_no_lint("lapply(x, function(xi) xi %in% tbl)", linter)
  # This one could not
  expect_no_lint("lapply(x, function(xi) tbl %in% xi)", linter)
  # would require multiple lapply() loops
  expect_no_lint("lapply(x, function(xi) foo(bar(xi)))", linter)
  expect_no_lint("lapply(x, function(xi) return(foo(bar(xi))))", linter)

  # extractions, #2231
  expect_no_lint("lapply(l, function(x) rle(x)$values)", linter)
  expect_no_lint('lapply(l, function(x) rle(x)["values"])', linter)
  expect_no_lint('lapply(l, function(x) rle(x)[["values"]])', linter)
  expect_no_lint("lapply(l, function(x) rle(x)@values)", linter)

  # return() extractions, #2258
  expect_no_lint("lapply(l, function(x) return(foo(x)$bar))", linter)
  expect_no_lint('lapply(l, function(x) return(rle(x)["values"]))', linter)
  expect_no_lint('lapply(l, function(x) return(rle(x)[["values"]]))', linter)
  expect_no_lint("lapply(l, function(x) return(rle(x)@values))", linter)

  # Other operators, #2247
  expect_no_lint("lapply(l, function(x) foo(x) - 1)", linter)
  expect_no_lint("lapply(l, function(x) foo(x) * 2)", linter)
  expect_no_lint("lapply(l, function(x) foo(x) ^ 3)", linter)
  expect_no_lint("lapply(l, function(x) foo(x) %% 4)", linter)

  # Don't include other lambdas, #2249
  expect_no_lint(
    trim_some('{
      lapply(x, function(e) sprintf("%o", e))
      lapply(y, function(e) paste(strlpad(e, "0", width)))
    }'),
    linter
  )

  # only call is on RHS of operator, #2310
  expect_no_lint("lapply(l, function(x) 'a' %in% names(x))", linter)
  expect_no_lint("lapply(l, function(x = 1) 'a' %in% names(x))", linter)
})

test_that("unnecessary_lambda_linter skips allowed inner comparisons", {
  linter <- unnecessary_lambda_linter()

  # lapply returns a list, so not the same, though as.list is probably
  #   a better choice
  expect_no_lint("lapply(x, function(xi) foo(xi) == 2)", linter)

  # this _may_ return a matrix, though outer is probably a better choice if so
  expect_no_lint("sapply(x, function(xi) foo(xi) == y)", linter)

  # only lint "plain" calls that can be replaced by eliminating the lambda
  expect_no_lint("sapply(x, function(xi) sum(abs(xi)) == 0)", linter)
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

  expect_no_lint("sapply(x, function(xi) foo(xi) == 2)", linter_allow)
  expect_no_lint("sapply(x, function(xi) foo(xi) == 'a')", linter_allow)
  expect_no_lint("sapply(x, function(xi) foo(xi) == 1 + 2i)", linter_allow)

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

  expect_no_lint("sapply(x, function(xi) foo(xi) >= 2)", linter_allow)
  expect_no_lint("sapply(x, function(xi) foo(xi) != 'a')", linter_allow)
  expect_no_lint("sapply(x, function(xi) foo(xi) < 1 + 2i)", linter_allow)
})

test_that("unnecessary_lambda_linter doesn't apply to keyword args", {
  linter <- unnecessary_lambda_linter()

  expect_no_lint("lapply(x, function(xi) data.frame(nm = xi))", linter)
  expect_no_lint("lapply(x, function(xi) return(data.frame(nm = xi)))", linter)
})

test_that("purrr-style anonymous functions are also caught", {
  linter <- unnecessary_lambda_linter()

  expect_no_lint("purrr::map(x, ~.x)", linter)
  expect_no_lint("purrr::map_df(x, ~lm(y, .x))", linter)
  expect_no_lint("map_dbl(x, ~foo(bar = .x))", linter)

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

  expect_no_lint(
    trim_some("
      lapply(x, function(xi) {
        print(xi)
        xi
      })
    "),
    linter
  )

  # false positives like #2231, #2247 are avoided with braces too
  expect_no_lint("lapply(x, function(xi) { foo(xi)$bar })", linter)
  expect_no_lint("lapply(x, function(xi) { foo(xi) - 1 })", linter)
})

test_that("function shorthand is handled", {
  skip_if_not_r_version("4.1.0")
  linter <- unnecessary_lambda_linter()
  linter_allow <- unnecessary_lambda_linter(allow_comparison = TRUE)

  expect_lint(
    "lapply(DF, \\(x) sum(x))",
    rex::rex("Pass sum directly as a symbol to lapply()"),
    linter
  )

  lint_msg <- rex::rex("Compare to a constant after calling sapply() to get", anything, "sapply(x, foo)")
  expect_lint("sapply(x, \\(xi) foo(xi) == 2)", lint_msg, linter)
  expect_lint("sapply(x, \\(xi) foo(xi) == 'a')", lint_msg, linter)
  expect_lint("sapply(x, \\(xi) foo(xi) == 1 + 2i)", lint_msg, linter)

  expect_no_lint("sapply(x, \\(xi) foo(xi) == 2)", linter_allow)
  expect_no_lint("sapply(x, \\(xi) foo(xi) == 'a')", linter_allow)
  expect_no_lint("sapply(x, \\(xi) foo(xi) == 1 + 2i)", linter_allow)
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
