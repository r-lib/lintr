# Object length linter

Check that object names are not too long. The length of an object name
is defined as the length in characters, after removing extraneous parts:

## Usage

``` r
object_length_linter(length = 30L)
```

## Arguments

- length:

  maximum variable name length allowed.

## Details

- generic prefixes for implementations of S3 generics, e.g.
  `as.data.frame.my_class` has length 8.

- leading `.`, e.g. `.my_hidden_function` has length 18.

- "%%" for infix operators, e.g. `%my_op%` has length 5.

- trailing `<-` for assignment functions, e.g. `my_attr<-` has length 7.

Note that this behavior relies in part on having packages in your
Imports available; see the detailed note in
[`object_name_linter()`](https://lintr.r-lib.org/dev/reference/object_name_linter.md)
for more details.

## See also

[linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
complete list of linters available in lintr.

## Tags

[configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md),
[default](https://lintr.r-lib.org/dev/reference/default_linters.md),
[executing](https://lintr.r-lib.org/dev/reference/executing_linters.md),
[readability](https://lintr.r-lib.org/dev/reference/readability_linters.md),
[style](https://lintr.r-lib.org/dev/reference/style_linters.md)

## Examples

``` r
# will produce lints
lint(
  text = "very_very_long_variable_name <- 1L",
  linters = object_length_linter(length = 10L)
)
#> <text>:1:1: style: [object_length_linter] Variable and function names should not be longer than 10 characters.
#> very_very_long_variable_name <- 1L
#> ^~~~~~~~~~~~~~~~~~~~~~~~~~~~

# okay
lint(
  text = "very_very_long_variable_name <- 1L",
  linters = object_length_linter(length = 30L)
)
#> ℹ No lints found.

lint(
  text = "var <- 1L",
  linters = object_length_linter(length = 10L)
)
#> ℹ No lints found.
```
