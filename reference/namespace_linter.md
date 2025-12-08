# Namespace linter

Check for missing packages and symbols in namespace calls. Note that
using `check_exports=TRUE` or `check_nonexports=TRUE` will load packages
used in user code so it could potentially change the global state.

## Usage

``` r
namespace_linter(check_exports = TRUE, check_nonexports = TRUE)
```

## Arguments

- check_exports:

  Check if `symbol` is exported from `namespace` in `namespace::symbol`
  calls.

- check_nonexports:

  Check if `symbol` exists in `namespace` in `namespace:::symbol` calls.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[correctness](https://lintr.r-lib.org/reference/correctness_linters.md),
[executing](https://lintr.r-lib.org/reference/executing_linters.md),
[robustness](https://lintr.r-lib.org/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "xyzxyz::sd(c(1, 2, 3))",
  linters = namespace_linter()
)
#> <text>:1:1: warning: [namespace_linter] Package 'xyzxyz' is not installed.
#> xyzxyz::sd(c(1, 2, 3))
#> ^~~~~~

lint(
  text = "stats::ssd(c(1, 2, 3))",
  linters = namespace_linter()
)
#> <text>:1:8: warning: [namespace_linter] 'ssd' is not exported from {stats}.
#> stats::ssd(c(1, 2, 3))
#>        ^~~

# okay
lint(
  text = "stats::sd(c(1, 2, 3))",
  linters = namespace_linter()
)
#> ℹ No lints found.

lint(
  text = "stats::ssd(c(1, 2, 3))",
  linters = namespace_linter(check_exports = FALSE)
)
#> ℹ No lints found.

lint(
  text = "stats:::ssd(c(1, 2, 3))",
  linters = namespace_linter(check_nonexports = FALSE)
)
#> ℹ No lints found.
```
