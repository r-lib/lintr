test_that("unnecessary_lambda_linter skips allowed usages", {
  linter <- unnecessary_lambda_linter()
  expect_lint("lapply(DF, sum)", NULL, linter)
  expect_lint("apply(M, 1, sum, na.rm = TRUE)", NULL, linter)

  # the first argument may be ... or have a cumbersome name, so an anonymous
  #   function may be preferable (e.g. this is often the case for grep() calls)
  expect_lint("sapply(x, function(xi) foo(1, xi))", NULL, linter)

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
  # TODO(michaelchirico): I believe there's cases where it's impossible to avoid an anonymous function due to a
  #   conflict where you have to pass FUN= to an inner *apply function but it gets interpreted as the outer *apply's FUN
  #   argument... but it's escaping me now.
})

test_that("unnecessary_lambda_linter blocks simple disallowed usage", {
  expect_lint(
    "lapply(DF, function(x) sum(x))",
    rex::rex("Pass sum directly as a symbol to lapply()"),
    unnecessary_lambda_linter()
  )

  expect_lint(
    "rapply(l, function(x) is.data.frame(x))",
    rex::rex("Pass is.data.frame directly as a symbol to rapply()"),
    unnecessary_lambda_linter()
  )

  expect_lint(
    "eapply(env, function(x) sum(x, na.rm = TRUE))",
    rex::rex("Pass sum directly as a symbol to eapply()"),
    unnecessary_lambda_linter()
  )
})

test_that("unnecessary_lambda_linter doesn't apply to keyword args", {
  expect_lint("lapply(x, function(xi) data.frame(nm = xi))", NULL, unnecessary_lambda_linter())
})

test_that("purrr-style anonymous functions are also caught", {
  # TODO(michaelchirico): this is just purrr::flatten(x). We should write another
  #   linter to encourage that usage.
  expect_lint("purrr::map(x, ~.x)", NULL, unnecessary_lambda_linter())
  expect_lint("purrr::map_df(x, ~lm(y, .x))", NULL, unnecessary_lambda_linter())
  expect_lint("map_dbl(x, ~foo(bar = .x))", NULL, unnecessary_lambda_linter())

  expect_lint(
    "purrr::map(x, ~foo(.x))",
    rex::rex("Pass foo directly as a symbol to map()"),
    unnecessary_lambda_linter()
  )
  expect_lint(
    "purrr::map_int(x, ~foo(.x, y))",
    rex::rex("Pass foo directly as a symbol to map_int()"),
    unnecessary_lambda_linter()
  )
})
