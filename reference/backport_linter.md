# Backport linter

Check for usage of unavailable functions. Not reliable for testing
r-devel dependencies.

## Usage

``` r
backport_linter(r_version = getRversion(), except = character())
```

## Arguments

- r_version:

  Minimum R version to test for compatibility. Defaults to the R version
  currently in use. The version can be specified as a version number, or
  as a version alias (such as `"devel"`, `"oldrel"`, `"oldrel-1"`).

- except:

  Character vector of functions to be excluded from linting. Use this to
  list explicitly defined backports, e.g. those imported from the
  `{backports}` package or manually defined in your package.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[package_development](https://lintr.r-lib.org/reference/package_development_linters.md),
[robustness](https://lintr.r-lib.org/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "trimws(x)",
  linters = backport_linter("3.0.0")
)
#> <text>:1:1: warning: [backport_linter] trimws (R 3.2.0) is not always available for requested dependency (R >= 3.0.0).
#> trimws(x)
#> ^~~~~~

lint(
  text = "str2lang(x)",
  linters = backport_linter("3.2.0")
)
#> <text>:1:1: warning: [backport_linter] str2lang (R 3.6.0) is not always available for requested dependency (R >= 3.2.0).
#> str2lang(x)
#> ^~~~~~~~

lint(
  text = "deparse1(expr)",
  linters = backport_linter("3.6.0")
)
#> <text>:1:1: warning: [backport_linter] deparse1 (R 4.0.0) is not always available for requested dependency (R >= 3.6.0).
#> deparse1(expr)
#> ^~~~~~~~

# okay
lint(
  text = "trimws(x)",
  linters = backport_linter("3.6.0")
)
#> ℹ No lints found.

lint(
  text = "str2lang(x)",
  linters = backport_linter("3.2.0", except = "str2lang")
)
#> ℹ No lints found.

# Version aliases instead of numbers can also be passed to `r_version`
lint(
  text = "deparse1(expr)",
  linters = backport_linter("release")
)
#> ℹ No lints found.
```
