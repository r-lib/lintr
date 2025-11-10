# Block usage of `==`, `!=` on logical vectors

Testing `x == TRUE` is redundant if `x` is a logical vector. Wherever
this is used to improve readability, the solution should instead be to
improve the naming of the object to better indicate that its contents
are logical. This can be done using prefixes (is, has, can, etc.). For
example, `is_child`, `has_parent_supervision`, `can_watch_horror_movie`
clarify their logical nature, while `child`, `parent_supervision`,
`watch_horror_movie` don't.

## Usage

``` r
redundant_equals_linter()
```

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- [`outer_negation_linter()`](https://lintr.r-lib.org/dev/reference/outer_negation_linter.md)

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[common_mistakes](https://lintr.r-lib.org/dev/reference/common_mistakes_linters.md),
[efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "if (any(x == TRUE)) 1",
  linters = redundant_equals_linter()
)
#> <text>:1:9: warning: [redundant_equals_linter] Using == on a logical vector is redundant. Well-named logical vectors can be used directly in filtering. For data.table's `i` argument, wrap the column name in (), like `DT[(is_treatment)]`.
#> if (any(x == TRUE)) 1
#>         ^~~~~~~~~

lint(
  text = "if (any(x != FALSE)) 0",
  linters = redundant_equals_linter()
)
#> <text>:1:9: warning: [redundant_equals_linter] Using != on a logical vector is redundant. Well-named logical vectors can be used directly in filtering. For data.table's `i` argument, wrap the column name in (), like `DT[(is_treatment)]`.
#> if (any(x != FALSE)) 0
#>         ^~~~~~~~~~

lint(
  text = "dt[is_tall == FALSE, y]",
  linters = redundant_equals_linter()
)
#> <text>:1:4: warning: [redundant_equals_linter] Using == on a logical vector is redundant. Well-named logical vectors can be used directly in filtering. For data.table's `i` argument, wrap the column name in (), like `DT[(is_treatment)]`.
#> dt[is_tall == FALSE, y]
#>    ^~~~~~~~~~~~~~~~

# okay
lint(
  text = "if (any(x)) 1",
  linters = redundant_equals_linter()
)
#> ℹ No lints found.

lint(
  text = "if (!all(x)) 0",
  linters = redundant_equals_linter()
)
#> ℹ No lints found.

# in `{data.table}` semantics, `dt[x]` is a join, `dt[(x)]` is a subset
lint(
  text = "dt[(!is_tall), y]",
  linters = redundant_equals_linter()
)
#> ℹ No lints found.
```
