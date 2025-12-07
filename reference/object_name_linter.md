# Object name linter

Check that object names conform to a naming style. The default naming
styles are "snake_case" and "symbols".

## Usage

``` r
object_name_linter(styles = c("snake_case", "symbols"), regexes = character())
```

## Arguments

- styles:

  A subset of ‘symbols’, ‘CamelCase’, ‘camelCase’, ‘snake_case’,
  ‘SNAKE_CASE’, ‘dotted.case’, ‘lowercase’, ‘UPPERCASE’ . A name should
  match at least one of these styles. The `"symbols"` style refers to
  names containing *only* non-alphanumeric characters; e.g., defining
  `%+%` from ggplot2 or `%>%` from magrittr would not generate lint
  markers, whereas `%m+%` from lubridate (containing both alphanumeric
  *and* non-alphanumeric characters) would.

- regexes:

  A (possibly named) character vector specifying a custom naming
  convention. If named, the names will be used in the lint message.
  Otherwise, the regexes enclosed by `/` will be used in the lint
  message. Note that specifying `regexes` overrides the default
  `styles`. So if you want to combine `regexes` and `styles`, both need
  to be explicitly specified.

## Details

Quotes (`` `"' ``) and specials (`%` and trailing `<-`) are not
considered part of the object name.

Note when used in a package, in order to ignore objects imported from
other namespaces, this linter will attempt
[`getNamespaceExports()`](https://rdrr.io/r/base/ns-reflect.html)
whenever an `import(PKG)` or `importFrom(PKG, ...)` statement is found
in your NAMESPACE file. If
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) fails (e.g.,
the package is not yet installed), the linter won't be able to ignore
some usages that would otherwise be allowed.

Suppose, for example, you have `import(upstream)` in your NAMESPACE,
which makes available its exported S3 generic function
`a_really_quite_long_function_name` that you then extend in your package
by defining a corresponding method for your class `my_class`. Then, if
`upstream` is not installed when this linter runs, a lint will be thrown
on this object (even though you don't "own" its full name).

The best way to get lintr to work correctly is to install the package so
that it's available in the session where this linter is running.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/reference/configurable_linters.md),
[consistency](https://lintr.r-lib.org/reference/consistency_linters.md),
[default](https://lintr.r-lib.org/reference/default_linters.md),
[executing](https://lintr.r-lib.org/reference/executing_linters.md),
[style](https://lintr.r-lib.org/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "my_var <- 1L",
  linters = object_name_linter(styles = "CamelCase")
)
#> <text>:1:1: style: [object_name_linter] Variable and function name style should match CamelCase.
#> my_var <- 1L
#> ^~~~~~

lint(
  text = "xYz <- 1L",
  linters = object_name_linter(styles = c("UPPERCASE", "lowercase"))
)
#> <text>:1:1: style: [object_name_linter] Variable and function name style should match UPPERCASE or lowercase.
#> xYz <- 1L
#> ^~~

lint(
  text = "MyVar <- 1L",
  linters = object_name_linter(styles = "dotted.case")
)
#> <text>:1:1: style: [object_name_linter] Variable and function name style should match dotted.case.
#> MyVar <- 1L
#> ^~~~~

lint(
  text = "asd <- 1L",
  linters = object_name_linter(regexes = c(my_style = "F$", "f$"))
)
#> <text>:1:1: style: [object_name_linter] Variable and function name style should match my_style or /f$/.
#> asd <- 1L
#> ^~~

# okay
lint(
  text = "my_var <- 1L",
  linters = object_name_linter(styles = "snake_case")
)
#> ℹ No lints found.

lint(
  text = "xyz <- 1L",
  linters = object_name_linter(styles = "lowercase")
)
#> ℹ No lints found.

lint(
  text = "my.var <- 1L; myvar <- 2L",
  linters = object_name_linter(styles = c("dotted.case", "lowercase"))
)
#> ℹ No lints found.

lint(
  text = "asdf <- 1L; asdF <- 1L",
  linters = object_name_linter(regexes = c(my_style = "F$", "f$"))
)
#> ℹ No lints found.
```
