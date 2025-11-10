# Require usage of correctly-typed literals over literal coercions

`as.integer(1)` (or `rlang::int(1)`) is the same as `1L` but the latter
is more concise and gets typed correctly at compilation.

## Usage

``` r
literal_coercion_linter()
```

## Details

The same applies to missing sentinels like `NA` – typically, it is not
necessary to specify the storage type of `NA`, but when it is, prefer
using the typed version (e.g. `NA_real_`) instead of a coercion (like
`as.numeric(NA)`).

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "int(1)",
  linters = literal_coercion_linter()
)
#> <text>:1:1: warning: [literal_coercion_linter] Use 1L instead of int(1), i.e., use literals directly where possible, instead of coercion.
#> int(1)
#> ^~~~~~

lint(
  text = "as.character(NA)",
  linters = literal_coercion_linter()
)
#> <text>:1:1: warning: [literal_coercion_linter] Use NA_character_ instead of as.character(NA), i.e., use literals directly where possible, instead of coercion.
#> as.character(NA)
#> ^~~~~~~~~~~~~~~~

lint(
  text = "rlang::lgl(1L)",
  linters = literal_coercion_linter()
)
#> <text>:1:1: warning: [literal_coercion_linter] Use TRUE instead of rlang::lgl(1L), i.e., use literals directly where possible, instead of coercion.
#> rlang::lgl(1L)
#> ^~~~~~~~~~~~~~

# okay
lint(
  text = "1L",
  linters = literal_coercion_linter()
)
#> ℹ No lints found.

lint(
  text = "NA_character_",
  linters = literal_coercion_linter()
)
#> ℹ No lints found.

lint(
  text = "TRUE",
  linters = literal_coercion_linter()
)
#> ℹ No lints found.
```
