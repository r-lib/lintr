# Block assigning any variables whose name clashes with a `base` R function

Re-using existing names creates a risk of subtle error best avoided.
Avoiding this practice also encourages using better, more descriptive
names.

## Usage

``` r
object_overwrite_linter(
  packages = c("base", "stats", "utils", "tools", "methods", "graphics", "grDevices"),
  allow_names = character()
)
```

## Arguments

- packages:

  Character vector of packages to search for names that should be
  avoided. Defaults to the most common default packages: base, stats,
  utils, tools, methods, graphics, and grDevices.

- allow_names:

  Character vector of object names to ignore, i.e., which are allowed to
  collide with exports from `packages`.

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://style.tidyverse.org/syntax.html#object-names>

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[executing](https://lintr.r-lib.org/dev/reference/executing_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md)

## Examples

``` r
# will produce lints
code <- "function(x) {\n  data <- x\n  data\n}"
writeLines(code)
#> function(x) {
#>   data <- x
#>   data
#> }
lint(
  text = code,
  linters = object_overwrite_linter()
)
#> <text>:2:3: warning: [object_overwrite_linter] 'data' is an exported object from package 'utils'. Avoid re-using such symbols.
#>   data <- x
#>   ^~~~

code <- "function(x) {\n  lint <- 'fun'\n  lint\n}"
writeLines(code)
#> function(x) {
#>   lint <- 'fun'
#>   lint
#> }
lint(
  text = code,
  linters = object_overwrite_linter(packages = "lintr")
)
#> <text>:2:3: warning: [object_overwrite_linter] 'lint' is an exported object from package 'lintr'. Avoid re-using such symbols.
#>   lint <- 'fun'
#>   ^~~~

# okay
code <- "function(x) {\n  data('mtcars')\n}"
writeLines(code)
#> function(x) {
#>   data('mtcars')
#> }
lint(
  text = code,
  linters = object_overwrite_linter()
)
#> ℹ No lints found.

code <- "function(x) {\n  data <- x\n  data\n}"
writeLines(code)
#> function(x) {
#>   data <- x
#>   data
#> }
lint(
  text = code,
  linters = object_overwrite_linter(packages = "base")
)
#> ℹ No lints found.

# names in function signatures are ignored
lint(
  text = "function(data) data <- subset(data, x > 0)",
  linters = object_overwrite_linter()
)
#> ℹ No lints found.
```
