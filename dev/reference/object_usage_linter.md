# Object usage linter

Check that closures have the proper usage using
[`codetools::checkUsage()`](https://rdrr.io/pkg/codetools/man/checkUsage.html).
Note that this runs [`base::eval()`](https://rdrr.io/r/base/eval.html)
on the code, so **do not use with untrusted code**.

## Usage

``` r
object_usage_linter(
  interpret_glue = NULL,
  interpret_extensions = c("glue", "rlang"),
  skip_with = TRUE
)
```

## Arguments

- interpret_glue:

  (Deprecated) If `TRUE`, interpret
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) calls
  to avoid false positives caused by local variables which are only used
  in a glue expression. Provide `interpret_extensions` instead, see
  below.

- interpret_extensions:

  Character vector of extensions to interpret. These are meant to cover
  known cases where variables may be used in ways understood by the
  reader but not by `checkUsage()` to avoid false positives. Currently
  `"glue"` and `"rlang"` are supported, both of which are in the
  default.

  - For `glue`, examine
    [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
    calls.

  - For `rlang`, examine `.env$key` usages.

- skip_with:

  A logical. If `TRUE` (default), code in
  [`with()`](https://rdrr.io/r/base/with.html) expressions will be
  skipped. This argument will be passed to `skipWith` argument of
  [`codetools::checkUsage()`](https://rdrr.io/pkg/codetools/man/checkUsage.html).

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Linters

The following linters are tagged with 'package_development':

- [`backport_linter`](https://lintr.r-lib.org/dev/reference/backport_linter.md)

- [`conjunct_test_linter`](https://lintr.r-lib.org/dev/reference/conjunct_test_linter.md)

- [`expect_comparison_linter`](https://lintr.r-lib.org/dev/reference/expect_comparison_linter.md)

- [`expect_identical_linter`](https://lintr.r-lib.org/dev/reference/expect_identical_linter.md)

- [`expect_length_linter`](https://lintr.r-lib.org/dev/reference/expect_length_linter.md)

- [`expect_named_linter`](https://lintr.r-lib.org/dev/reference/expect_named_linter.md)

- [`expect_not_linter`](https://lintr.r-lib.org/dev/reference/expect_not_linter.md)

- [`expect_null_linter`](https://lintr.r-lib.org/dev/reference/expect_null_linter.md)

- [`expect_s3_class_linter`](https://lintr.r-lib.org/dev/reference/expect_s3_class_linter.md)

- [`expect_s4_class_linter`](https://lintr.r-lib.org/dev/reference/expect_s4_class_linter.md)

- [`expect_true_false_linter`](https://lintr.r-lib.org/dev/reference/expect_true_false_linter.md)

- [`expect_type_linter`](https://lintr.r-lib.org/dev/reference/expect_type_linter.md)

- [`package_hooks_linter`](https://lintr.r-lib.org/dev/reference/package_hooks_linter.md)

- [`yoda_test_linter`](https://lintr.r-lib.org/dev/reference/yoda_test_linter.md)

## Examples

``` r
# will produce lints
lint(
  text = "foo <- function() { x <- 1 }",
  linters = object_usage_linter()
)
#> <text>:1:21: warning: [object_usage_linter] local variable 'x' assigned but may not be used
#> foo <- function() { x <- 1 }
#>                     ^

# okay
lint(
  text = "foo <- function(x) { x <- 1 }",
  linters = object_usage_linter()
)
#> ℹ No lints found.

lint(
  text = "foo <- function() { x <- 1; return(x) }",
  linters = object_usage_linter()
)
#> ℹ No lints found.
```
