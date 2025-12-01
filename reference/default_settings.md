# Default lintr settings

The default settings consist of

- `linters`: a list of default linters (see
  [`default_linters()`](https://lintr.r-lib.org/reference/default_linters.md))

- `encoding`: the character encoding assumed for the file

- `exclude`: pattern used to exclude a line of code

- `exclude_start`, `exclude_end`: patterns used to mark start and end of
  the code block to exclude

- `exclude_linter`, `exclude_linter_sep`: patterns used to exclude
  linters

- `exclusions`: a list of exclusions, see
  [`exclude()`](https://lintr.r-lib.org/reference/exclude.md) for a
  complete description of valid values.

- `cache_directory`: location of cache directory

- `error_on_lint`: decides if error should be produced when any lints
  are found

There are no settings without defaults, i.e., this list describes every
valid setting.

## Usage

``` r
default_settings
```

## Format

An object of class `list` of length 11.

## See also

[`read_settings()`](https://lintr.r-lib.org/reference/read_settings.md),
[default_linters](https://lintr.r-lib.org/reference/default_linters.md)

## Examples

``` r
# available settings
names(default_settings)
#>  [1] "linters"            "encoding"           "exclude"           
#>  [4] "exclude_next"       "exclude_start"      "exclude_end"       
#>  [7] "exclude_linter"     "exclude_linter_sep" "exclusions"        
#> [10] "cache_directory"    "error_on_lint"     

# linters included by default
names(default_settings$linters)
#>  [1] "assignment_linter"                "brace_linter"                    
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

# default values for a few of the other settings
default_settings[c(
  "encoding",
  "exclude",
  "exclude_start",
  "exclude_end",
  "exclude_linter",
  "exclude_linter_sep",
  "exclusions",
  "error_on_lint"
)]
#> $encoding
#> [1] "UTF-8"
#> 
#> $exclude
#> #[[:space:]]*nolint
#> 
#> $exclude_start
#> #[[:space:]]*nolint start
#> 
#> $exclude_end
#> #[[:space:]]*nolint end
#> 
#> $exclude_linter
#> ^[[:space:]]*:[[:space:]]*(?<linters>(?:(?:[^,.])+[[:space:]]*,[[:space:]]*)*(?:[^,.])+)\.
#> 
#> $exclude_linter_sep
#> [[:space:]]*,[[:space:]]*
#> 
#> $exclusions
#> list()
#> 
#> $error_on_lint
#> [1] FALSE
#> 
```
