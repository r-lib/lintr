# Test that the package is lint free

This function is a thin wrapper around lint_package that simply tests
there are no lints in the package. It can be used to ensure that your
tests fail if the package contains lints.

## Usage

``` r
expect_lint_free(...)
```

## Arguments

- ...:

  arguments passed to
  [`lint_package()`](https://lintr.r-lib.org/reference/lint.md)
