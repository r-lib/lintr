# Use lintr in your project

Create a minimal lintr config file as a starting point for customization

## Usage

``` r
use_lintr(path = ".", type = c("tidyverse", "full"))
```

## Arguments

- path:

  Path to project root, where a `.lintr` file should be created. If the
  `.lintr` file already exists, an error will be thrown.

- type:

  What kind of configuration to create?

  - `tidyverse` creates a minimal lintr config, based on the default
    linters
    ([`linters_with_defaults()`](https://lintr.r-lib.org/reference/linters_with_defaults.md)).
    These are suitable for following [the tidyverse style
    guide](https://style.tidyverse.org/).

  - `full` creates a lintr config using all available linters via
    [`all_linters()`](https://lintr.r-lib.org/reference/all_linters.md).

## Value

Path to the generated configuration, invisibly.

## See also

[`vignette("lintr")`](https://lintr.r-lib.org/articles/lintr.md) for
detailed introduction to using and configuring lintr.

## Examples

``` r
if (FALSE) {
  # use the default set of linters
  lintr::use_lintr()
  # or try all linters
  lintr::use_lintr(type = "full")

  # then
  lintr::lint_dir()
}
```
