# Cyclomatic complexity linter

Check for overly complicated expressions. See `cyclocomp()` function
from `{cyclocomp}`.

## Usage

``` r
cyclocomp_linter(complexity_limit = 15L)
```

## Arguments

- complexity_limit:

  Maximum cyclomatic complexity, default `15`. Expressions more complex
  than this are linted.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "if (TRUE) 1 else 2",
  linters = cyclocomp_linter(complexity_limit = 1L)
)
#> <text>:1:1: style: [cyclocomp_linter] Reduce the cyclomatic complexity of this expression from 2 to at most 1. Consider replacing high-complexity sections like loops and branches with helper functions.
#> if (TRUE) 1 else 2
#> ^

# okay
lint(
  text = "if (TRUE) 1 else 2",
  linters = cyclocomp_linter(complexity_limit = 2L)
)
#> ℹ No lints found.
```
