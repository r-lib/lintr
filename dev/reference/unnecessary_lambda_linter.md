# Block usage of anonymous functions in iteration functions when unnecessary

Using an anonymous function in, e.g.,
[`lapply()`](https://rdrr.io/r/base/lapply.html) is not always
necessary, e.g. `lapply(DF, sum)` is the same as
`lapply(DF, function(x) sum(x))` and the former is more readable.

## Usage

``` r
unnecessary_lambda_linter(allow_comparison = FALSE)
```

## Arguments

- allow_comparison:

  Logical, default `FALSE`. If `TRUE`, lambdas like
  `function(x) foo(x) == 2`, where `foo` can be extracted to the
  "mapping" function and `==` vectorized instead of called repeatedly,
  are linted.

## Details

Cases like `lapply(x, \(xi) grep("ptn", xi))` are excluded because,
though the anonymous function *can* be avoided, doing so is not always
more readable.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "lapply(list(1:3, 2:4), function(xi) sum(xi))",
  linters = unnecessary_lambda_linter()
)
#> <text>:1:24: warning: [unnecessary_lambda_linter] Pass sum directly as a symbol to lapply() instead of wrapping it in an unnecessary anonymous function. For example, prefer lapply(DF, sum) to lapply(DF, function(x) sum(x)).
#> lapply(list(1:3, 2:4), function(xi) sum(xi))
#>                        ^~~~~~~~~~~~~~~~~~~~

lint(
  text = "sapply(x, function(xi) xi == 2)",
  linters = unnecessary_lambda_linter()
)
#> ℹ No lints found.

lint(
  text = "sapply(x, function(xi) sum(xi) > 0)",
  linters = unnecessary_lambda_linter()
)
#> <text>:1:24: warning: [unnecessary_lambda_linter] Compare to a constant after calling sapply() to get the full benefits of vectorization. Prefer sapply(x, foo) == 2 over sapply(x, function(xi) foo(xi) == 2, logical(1L)).
#> sapply(x, function(xi) sum(xi) > 0)
#>                        ^~~~~~~~~~~

# okay
lint(
  text = "lapply(list(1:3, 2:4), sum)",
  linters = unnecessary_lambda_linter()
)
#> ℹ No lints found.

lint(
  text = 'lapply(x, function(xi) grep("ptn", xi))',
  linters = unnecessary_lambda_linter()
)
#> ℹ No lints found.

lint(
  text = "lapply(x, function(xi) data.frame(col = xi))",
  linters = unnecessary_lambda_linter()
)
#> ℹ No lints found.

lint(
  text = "sapply(x, function(xi) xi == 2)",
  linters = unnecessary_lambda_linter(allow_comparison = TRUE)
)
#> ℹ No lints found.

lint(
  text = "sapply(x, function(xi) sum(xi) > 0)",
  linters = unnecessary_lambda_linter(allow_comparison = TRUE)
)
#> ℹ No lints found.

lint(
  text = "sapply(x, function(xi) sum(abs(xi)) > 10)",
  linters = unnecessary_lambda_linter()
)
#> ℹ No lints found.

lint(
  text = "sapply(x, sum) > 0",
  linters = unnecessary_lambda_linter()
)
#> ℹ No lints found.
```
