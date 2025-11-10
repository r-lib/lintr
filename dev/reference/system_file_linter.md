# Block usage of `file.path()` with `system.file()`

[`system.file()`](https://rdrr.io/r/base/system.file.html) has a `...`
argument which, internally, is passed to
[`file.path()`](https://rdrr.io/r/base/file.path.html), so including it
in user code is repetitive.

## Usage

``` r
system_file_linter()
```

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = 'system.file(file.path("path", "to", "data"), package = "foo")',
  linters = system_file_linter()
)
#> <text>:1:1: warning: [system_file_linter] Use the `...` argument of system.file() to expand paths, e.g. system.file("data", "model.csv", package = "myrf") instead of system.file(file.path("data", "model.csv"), package = "myrf")
#> system.file(file.path("path", "to", "data"), package = "foo")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lint(
  text = 'file.path(system.file(package = "foo"), "path", "to", "data")',
  linters = system_file_linter()
)
#> <text>:1:1: warning: [system_file_linter] Use the `...` argument of system.file() to expand paths, e.g. system.file("data", "model.csv", package = "myrf") instead of file.path(system.file(package = "myrf"), "data", "model.csv")
#> file.path(system.file(package = "foo"), "path", "to", "data")
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = 'system.file("path", "to", "data", package = "foo")',
  linters = system_file_linter()
)
#> ℹ No lints found.
```
