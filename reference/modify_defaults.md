# Modify lintr defaults

Modify a list of defaults by name, allowing for replacement, deletion
and addition of new elements.

## Usage

``` r
modify_defaults(defaults, ...)
```

## Arguments

- defaults:

  named list of elements to modify.

- ...:

  arguments of elements to change. If unnamed, the argument is
  automatically named. If the named argument already exists in
  `defaults`, it is replaced by the new element. If it does not exist,
  it is added. If the value is `NULL`, the element is removed.

## Value

A modified list of elements, sorted by name. To achieve this sort in a
platform-independent way, two transformations are applied to the names:
(1) replace `_` with `0` and (2) convert
[`tolower()`](https://rdrr.io/r/base/chartr.html).

## See also

- [linters_with_defaults](https://lintr.r-lib.org/reference/linters_with_defaults.md)
  for basing off lintr's set of default linters.

- [all_linters](https://lintr.r-lib.org/reference/all_linters.md) for
  basing off all available linters in lintr.

- [linters_with_tags](https://lintr.r-lib.org/reference/linters_with_tags.md)
  for basing off tags attached to linters, possibly across multiple
  packages.

- [available_linters](https://lintr.r-lib.org/reference/available_linters.md)
  to get a data frame of available linters.

- [linters](https://lintr.r-lib.org/reference/linters.md) for a complete
  list of linters available in lintr.

## Examples

``` r
# custom list of undesirable functions:
#    remove `sapply` (using `NULL`)
#    add `cat` (with an accompanying message),
#    add `print` (unnamed, i.e. with no accompanying message)
#    add `source` (as taken from `all_undesirable_functions`)
my_undesirable_functions <- modify_defaults(
  defaults = default_undesirable_functions,
  sapply = NULL, "cat" = "No cat allowed", "print", all_undesirable_functions[["source"]]
)

# list names of functions specified as undesirable
names(my_undesirable_functions)
#>  [1] ".libPaths"     "attach"        "browser"       "cat"          
#>  [5] "debug"         "debugcall"     "debugonce"     "detach"       
#>  [9] "library"       "mapply"        "options"       "par"          
#> [13] "print"         "require"       "setwd"         "sink"         
#> [17] "source"        "structure"     "Sys.setenv"    "Sys.setlocale"
#> [21] "trace"         "undebug"       "untrace"      
```
