# Check for a common mistake where a size check like 'length' is applied in the wrong place

Usage like `length(x == 0)` is a mistake. If you intended to check `x`
is empty, use `length(x) == 0`. Other mistakes are possible, but running
[`length()`](https://rdrr.io/r/base/length.html) on the outcome of a
logical comparison is never the best choice.

## Usage

``` r
length_test_linter()
```

## Details

The linter also checks for similar usage with
[`nrow()`](https://rdrr.io/r/base/nrow.html),
[`ncol()`](https://rdrr.io/r/base/nrow.html),
[`NROW()`](https://rdrr.io/r/base/nrow.html), and
[`NCOL()`](https://rdrr.io/r/base/nrow.html).

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "length(x == 0)",
  linters = length_test_linter()
)
#> <text>:1:1: warning: [length_test_linter] Checking the length of a logical vector is likely a mistake. Did you mean `length(x) == 0`?
#> length(x == 0)
#> ^~~~~~~~~~~~~~

lint(
  text = "nrow(x > 0) || ncol(x > 0)",
  linters = length_test_linter()
)
#> <text>:1:1: warning: [length_test_linter] Checking the nrow of a logical vector is likely a mistake. Did you mean `nrow(x) > 0`?
#> nrow(x > 0) || ncol(x > 0)
#> ^~~~~~~~~~~
#> <text>:1:16: warning: [length_test_linter] Checking the ncol of a logical vector is likely a mistake. Did you mean `ncol(x) > 0`?
#> nrow(x > 0) || ncol(x > 0)
#>                ^~~~~~~~~~~

lint(
  text = "NROW(x == 1) && NCOL(y == 1)",
  linters = length_test_linter()
)
#> <text>:1:1: warning: [length_test_linter] Checking the NROW of a logical vector is likely a mistake. Did you mean `NROW(x) == 1`?
#> NROW(x == 1) && NCOL(y == 1)
#> ^~~~~~~~~~~~
#> <text>:1:17: warning: [length_test_linter] Checking the NCOL of a logical vector is likely a mistake. Did you mean `NCOL(y) == 1`?
#> NROW(x == 1) && NCOL(y == 1)
#>                 ^~~~~~~~~~~~

# okay
lint(
  text = "length(x) > 0",
  linters = length_test_linter()
)
#> ℹ No lints found.

lint(
  text = "nrow(x) > 0 || ncol(x) > 0",
  linters = length_test_linter()
)
#> ℹ No lints found.

lint(
  text = "NROW(x) == 1 && NCOL(y) == 1",
  linters = length_test_linter()
)
#> ℹ No lints found.
```
