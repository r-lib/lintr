# Require usage of sample.int(n, m, ...) over sample(1:n, m, ...)

[`sample.int()`](https://rdrr.io/r/base/sample.html) is preferable to
[`sample()`](https://rdrr.io/r/base/sample.html) for the case of
sampling numbers between 1 and `n`. `sample` calls
[`sample.int()`](https://rdrr.io/r/base/sample.html) "under the hood".

## Usage

``` r
sample_int_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "sample(1:10, 2)",
  linters = sample_int_linter()
)
#> <text>:1:1: warning: [sample_int_linter] sample.int(n, m, ...) is preferable to sample(1:n, m, ...).
#> sample(1:10, 2)
#> ^~~~~~~~~~~~~~~

lint(
  text = "sample(seq(4), 2)",
  linters = sample_int_linter()
)
#> <text>:1:1: warning: [sample_int_linter] sample.int(n, m, ...) is preferable to sample(seq(n), m, ...).
#> sample(seq(4), 2)
#> ^~~~~~~~~~~~~~~~~

lint(
  text = "sample(seq_len(8), 2)",
  linters = sample_int_linter()
)
#> <text>:1:1: warning: [sample_int_linter] sample.int(n, m, ...) is preferable to sample(seq_len(n), m, ...).
#> sample(seq_len(8), 2)
#> ^~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "sample(seq(1, 5, by = 2), 2)",
  linters = sample_int_linter()
)
#> ℹ No lints found.

lint(
  text = "sample(letters, 2)",
  linters = sample_int_linter()
)
#> ℹ No lints found.
```
