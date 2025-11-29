# Get Linter metadata from a package

`available_linters()` obtains a tagged list of all Linters available in
a package.

`available_tags()` searches for available tags.

## Usage

``` r
available_linters(packages = "lintr", tags = NULL, exclude_tags = "deprecated")

available_tags(packages = "lintr")
```

## Arguments

- packages:

  A character vector of packages to search for linters.

- tags:

  Optional character vector of tags to search. Only linters with at
  least one matching tag will be returned. If `tags` is `NULL`, all
  linters will be returned. See `available_tags("lintr")` to find out
  what tags are already used by lintr.

- exclude_tags:

  Tags to exclude from the results. Linters with at least one matching
  tag will not be returned. If `exclude_tags` is `NULL`, no linters will
  be excluded. Note that `tags` takes priority, meaning that any tag
  found in both `tags` and `exclude_tags` will be included, not
  excluded. Note that linters with tag `"defunct"` (which do not work
  and can no longer be run) cannot be queried directly.

## Value

`available_linters` returns a data frame with columns 'linter',
'package' and 'tags':

- linter:

  A character column naming the function associated with the linter.

- package:

  A character column containing the name of the package providing the
  linter.

- tags:

  A list column containing tags associated with the linter.

`available_tags` returns a character vector of linter tags used by the
packages.

## Package Authors

To implement `available_linters()` for your package, include a file
`inst/lintr/linters.csv` in your package. The CSV file must contain the
columns 'linter' and 'tags', and be UTF-8 encoded. Additional columns
will be silently ignored if present and the columns are identified by
name. Each row describes a linter by

1.  its function name (e.g. `"assignment_linter"`) in the column
    'linter'.

2.  space-separated tags associated with the linter (e.g.
    `"style consistency default"`) in the column 'tags'.

Tags should be snake_case.

See `available_tags("lintr")` to find out what tags are already used by
lintr.

## See also

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

- `available_tags()` to retrieve the set of valid tags.

## Examples

``` r
lintr_linters <- available_linters()

# If the package doesn't exist or isn't installed, an empty data frame will be returned
available_linters("does-not-exist")
#> [1] linter  package tags   
#> <0 rows> (or 0-length row.names)

lintr_linters2 <- available_linters(c("lintr", "does-not-exist"))
identical(lintr_linters, lintr_linters2)
#> [1] TRUE
available_tags()
#>  [1] "best_practices"      "common_mistakes"     "configurable"       
#>  [4] "consistency"         "correctness"         "default"            
#>  [7] "efficiency"          "executing"           "package_development"
#> [10] "pkg_testthat"        "readability"         "regex"              
#> [13] "robustness"          "style"               "tidy_design"        
```
