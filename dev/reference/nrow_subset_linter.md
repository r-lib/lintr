# Block usage of `nrow(subset(x, .))`

Using `nrow(subset(x, condition))` to count the instances where
`condition` applies inefficiently requires doing a full subset of `x`
just to count the number of rows in the resulting subset. There are a
number of equivalent expressions that don't require the full subset,
e.g. `with(x, sum(condition))` (or, more generically,
`with(x, sum(condition, na.rm = TRUE))`).

## Usage

``` r
nrow_subset_linter()
```

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
  text = "nrow(subset(x, is_treatment))",
  linters = nrow_subset_linter()
)
#> <text>:1:1: warning: [nrow_subset_linter] Use arithmetic to count the number of rows satisfying a condition, rather than fully subsetting the data.frame and counting the resulting rows. For example, replace nrow(subset(x, is_treatment)) with sum(x$is_treatment). NB: use na.rm = TRUE if `is_treatment` has missing values.
#> nrow(subset(x, is_treatment))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "nrow(filter(x, is_treatment))",
  linters = nrow_subset_linter()
)
#> <text>:1:1: warning: [nrow_subset_linter] Use arithmetic to count the number of rows satisfying a condition, rather than fully subsetting the data.frame and counting the resulting rows. For example, replace nrow(subset(x, is_treatment)) with sum(x$is_treatment). NB: use na.rm = TRUE if `is_treatment` has missing values.
#> nrow(filter(x, is_treatment))
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = "x %>% filter(x, is_treatment) %>% nrow()",
  linters = nrow_subset_linter()
)
#> <text>:1:1: warning: [nrow_subset_linter] Use arithmetic to count the number of rows satisfying a condition, rather than fully subsetting the data.frame and counting the resulting rows. For example, replace nrow(subset(x, is_treatment)) with sum(x$is_treatment). NB: use na.rm = TRUE if `is_treatment` has missing values.
#> x %>% filter(x, is_treatment) %>% nrow()
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "with(x, sum(is_treatment, na.rm = TRUE))",
  linters = nrow_subset_linter()
)
#> ℹ No lints found.
```
