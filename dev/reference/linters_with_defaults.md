# Create a linter configuration based on defaults

Make a new list based on lintr's default linters. The result of this
function is meant to be passed to the `linters` argument of
[`lint()`](https://lintr.r-lib.org/dev/reference/lint.md), or to be put
in your configuration file.

## Usage

``` r
linters_with_defaults(..., defaults = default_linters)
```

## Arguments

- ...:

  Arguments of elements to change. If unnamed, the argument is
  automatically named. If the named argument already exists in the list
  of linters, it is replaced by the new element. If it does not exist,
  it is added. If the value is `NULL`, the linter is removed.

- defaults:

  Default list of linters to modify. Must be named.

## See also

- [linters_with_tags](https://lintr.r-lib.org/dev/reference/linters_with_tags.md)
  for basing off tags attached to linters, possibly across multiple
  packages.

- [all_linters](https://lintr.r-lib.org/dev/reference/all_linters.md)
  for basing off all available linters in lintr.

- [available_linters](https://lintr.r-lib.org/dev/reference/available_linters.md)
  to get a data frame of available linters.

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

## Examples

``` r
# When using interactively you will usually pass the result onto `lint` or `lint_package()`
f <- tempfile()
writeLines("my_slightly_long_variable_name <- 2.3", f)
lint(f, linters = linters_with_defaults(line_length_linter = line_length_linter(120L)))
#> ℹ No lints found.
unlink(f)

# the default linter list with a different line length cutoff
my_linters <- linters_with_defaults(line_length_linter = line_length_linter(120L))

# omit the argument name if you are just using different arguments
my_linters <- linters_with_defaults(defaults = my_linters, object_name_linter("camelCase"))

# remove assignment checks (with NULL), add absolute path checks
my_linters <- linters_with_defaults(
  defaults = my_linters,
  assignment_linter = NULL,
  absolute_path_linter()
)

# checking the included linters
names(my_linters)
#>  [1] "absolute_path_linter"             "brace_linter"                    
#>  [3] "commas_linter"                    "commented_code_linter"           
#>  [5] "equals_na_linter"                 "function_left_parentheses_linter"
#>  [7] "indentation_linter"               "infix_spaces_linter"             
#>  [9] "line_length_linter"               "object_length_linter"            
#> [11] "object_name_linter"               "object_usage_linter"             
#> [13] "paren_body_linter"                "pipe_consistency_linter"         
#> [15] "pipe_continuation_linter"         "quotes_linter"                   
#> [17] "return_linter"                    "semicolon_linter"                
#> [19] "seq_linter"                       "spaces_inside_linter"            
#> [21] "spaces_left_parentheses_linter"   "T_and_F_symbol_linter"           
#> [23] "trailing_blank_lines_linter"      "trailing_whitespace_linter"      
#> [25] "vector_logic_linter"              "whitespace_linter"               
```
