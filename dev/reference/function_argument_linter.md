# Function argument linter

Check that arguments with defaults come last in all function
declarations, as per the tidyverse design guide.

Changing the argument order can be a breaking change. An alternative to
changing the argument order is to instead set the default for such
arguments to `NULL`.

## Usage

``` r
function_argument_linter()
```

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- <https://design.tidyverse.org/required-no-defaults.html>

## Tags

[best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md),
[consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "function(y = 1, z = 2, x) {}",
  linters = function_argument_linter()
)
#> <text>:1:24: style: [function_argument_linter] Arguments without defaults should come before arguments with defaults.
#> function(y = 1, z = 2, x) {}
#>                        ^

lint(
  text = "function(x, y, z = 1, ..., w) {}",
  linters = function_argument_linter()
)
#> <text>:1:28: style: [function_argument_linter] Arguments without defaults should come before arguments with defaults.
#> function(x, y, z = 1, ..., w) {}
#>                            ^

# okay
lint(
  text = "function(x, y = 1, z = 2) {}",
  linters = function_argument_linter()
)
#> ℹ No lints found.

lint(
  text = "function(x, y, w, z = 1, ...) {}",
  linters = function_argument_linter()
)
#> ℹ No lints found.

lint(
  text = "function(y = 1, z = 2, x = NULL) {}",
  linters = function_argument_linter()
)
#> ℹ No lints found.

lint(
  text = "function(x, y, z = 1, ..., w = NULL) {}",
  linters = function_argument_linter()
)
#> ℹ No lints found.
```
