# Create a tag-based linter configuration

Make a new list based on all linters provided by `packages` and tagged
with `tags`. The result of this function is meant to be passed to the
`linters` argument of
[`lint()`](https://lintr.r-lib.org/dev/reference/lint.md), or to be put
in your configuration file.

## Usage

``` r
linters_with_tags(tags, ..., packages = "lintr", exclude_tags = "deprecated")
```

## Arguments

- tags:

  Optional character vector of tags to search. Only linters with at
  least one matching tag will be returned. If `tags` is `NULL`, all
  linters will be returned. See `available_tags("lintr")` to find out
  what tags are already used by lintr.

- ...:

  Arguments of elements to change. If unnamed, the argument is
  automatically named. If the named argument already exists in the list
  of linters, it is replaced by the new element. If it does not exist,
  it is added. If the value is `NULL`, the linter is removed.

- packages:

  A character vector of packages to search for linters.

- exclude_tags:

  Tags to exclude from the results. Linters with at least one matching
  tag will not be returned. If `exclude_tags` is `NULL`, no linters will
  be excluded. Note that `tags` takes priority, meaning that any tag
  found in both `tags` and `exclude_tags` will be included, not
  excluded. Note that linters with tag `"defunct"` (which do not work
  and can no longer be run) cannot be queried directly.

## Value

A modified list of linters.

## See also

- [linters_with_defaults](https://lintr.r-lib.org/dev/reference/linters_with_defaults.md)
  for basing off lintr's set of default linters.

- [all_linters](https://lintr.r-lib.org/dev/reference/all_linters.md)
  for basing off all available linters in lintr.

- [available_linters](https://lintr.r-lib.org/dev/reference/available_linters.md)
  to get a data frame of available linters.

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

## Examples

``` r
# `linters_with_defaults()` and `linters_with_tags("default")` are the same:
all.equal(linters_with_defaults(), linters_with_tags("default"))
#> [1] TRUE

# Get all linters useful for package development
linters <- linters_with_tags(tags = c("package_development", "style"))
names(linters)
#>  [1] "assignment_linter"                "backport_linter"                 
#>  [3] "brace_linter"                     "commas_linter"                   
#>  [5] "commented_code_linter"            "condition_call_linter"           
#>  [7] "conjunct_test_linter"             "consecutive_assertion_linter"    
#>  [9] "cyclocomp_linter"                 "expect_comparison_linter"        
#> [11] "expect_identical_linter"          "expect_length_linter"            
#> [13] "expect_named_linter"              "expect_not_linter"               
#> [15] "expect_null_linter"               "expect_s3_class_linter"          
#> [17] "expect_s4_class_linter"           "expect_shape_linter"             
#> [19] "expect_true_false_linter"         "expect_type_linter"              
#> [21] "function_argument_linter"         "function_left_parentheses_linter"
#> [23] "implicit_assignment_linter"       "implicit_integer_linter"         
#> [25] "indentation_linter"               "infix_spaces_linter"             
#> [27] "keyword_quote_linter"             "library_call_linter"             
#> [29] "line_length_linter"               "numeric_leading_zero_linter"     
#> [31] "object_length_linter"             "object_name_linter"              
#> [33] "object_usage_linter"              "one_call_pipe_linter"            
#> [35] "package_hooks_linter"             "paren_body_linter"               
#> [37] "pipe_call_linter"                 "pipe_consistency_linter"         
#> [39] "pipe_continuation_linter"         "quotes_linter"                   
#> [41] "repeat_linter"                    "return_linter"                   
#> [43] "semicolon_linter"                 "spaces_inside_linter"            
#> [45] "spaces_left_parentheses_linter"   "T_and_F_symbol_linter"           
#> [47] "todo_comment_linter"              "trailing_blank_lines_linter"     
#> [49] "trailing_whitespace_linter"       "undesirable_function_linter"     
#> [51] "undesirable_operator_linter"      "unnecessary_concatenation_linter"
#> [53] "whitespace_linter"                "yoda_test_linter"                

# Get all linters tagged as "default" from lintr and mypkg
if (FALSE) {
  linters_with_tags("default", packages = c("lintr", "mypkg"))
}
```
