# Create a linter configuration based on all available linters

Create a linter configuration based on all available linters

## Usage

``` r
all_linters(..., packages = "lintr")
```

## Arguments

- ...:

  Arguments of elements to change. If unnamed, the argument is
  automatically named. If the named argument already exists in the list
  of linters, it is replaced by the new element. If it does not exist,
  it is added. If the value is `NULL`, the linter is removed.

- packages:

  A character vector of packages to search for linters.

## See also

- [linters_with_defaults](https://lintr.r-lib.org/dev/reference/linters_with_defaults.md)
  for basing off lintr's set of default linters.

- [linters_with_tags](https://lintr.r-lib.org/dev/reference/linters_with_tags.md)
  for basing off tags attached to linters, possibly across multiple
  packages.

- [available_linters](https://lintr.r-lib.org/dev/reference/available_linters.md)
  to get a data frame of available linters.

- [linters](https://lintr.r-lib.org/dev/reference/linters.md) for a
  complete list of linters available in lintr.

## Examples

``` r
names(all_linters())
#>   [1] "absolute_path_linter"             "all_equal_linter"                
#>   [3] "any_duplicated_linter"            "any_is_na_linter"                
#>   [5] "assignment_linter"                "backport_linter"                 
#>   [7] "boolean_arithmetic_linter"        "brace_linter"                    
#>   [9] "class_equals_linter"              "coalesce_linter"                 
#>  [11] "commas_linter"                    "commented_code_linter"           
#>  [13] "comparison_negation_linter"       "condition_call_linter"           
#>  [15] "condition_message_linter"         "conjunct_test_linter"            
#>  [17] "consecutive_assertion_linter"     "consecutive_mutate_linter"       
#>  [19] "cyclocomp_linter"                 "download_file_linter"            
#>  [21] "duplicate_argument_linter"        "empty_assignment_linter"         
#>  [23] "equals_na_linter"                 "expect_comparison_linter"        
#>  [25] "expect_identical_linter"          "expect_length_linter"            
#>  [27] "expect_named_linter"              "expect_not_linter"               
#>  [29] "expect_null_linter"               "expect_s3_class_linter"          
#>  [31] "expect_s4_class_linter"           "expect_shape_linter"             
#>  [33] "expect_true_false_linter"         "expect_type_linter"              
#>  [35] "fixed_regex_linter"               "for_loop_index_linter"           
#>  [37] "function_argument_linter"         "function_left_parentheses_linter"
#>  [39] "function_return_linter"           "if_not_else_linter"              
#>  [41] "if_switch_linter"                 "ifelse_censor_linter"            
#>  [43] "implicit_assignment_linter"       "implicit_integer_linter"         
#>  [45] "indentation_linter"               "infix_spaces_linter"             
#>  [47] "inner_combine_linter"             "is_numeric_linter"               
#>  [49] "keyword_quote_linter"             "length_levels_linter"            
#>  [51] "length_test_linter"               "lengths_linter"                  
#>  [53] "library_call_linter"              "line_length_linter"              
#>  [55] "list2df_linter"                   "list_comparison_linter"          
#>  [57] "literal_coercion_linter"          "matrix_apply_linter"             
#>  [59] "missing_argument_linter"          "missing_package_linter"          
#>  [61] "namespace_linter"                 "nested_ifelse_linter"            
#>  [63] "nested_pipe_linter"               "nonportable_path_linter"         
#>  [65] "nrow_subset_linter"               "numeric_leading_zero_linter"     
#>  [67] "nzchar_linter"                    "object_length_linter"            
#>  [69] "object_name_linter"               "object_overwrite_linter"         
#>  [71] "object_usage_linter"              "one_call_pipe_linter"            
#>  [73] "outer_negation_linter"            "package_hooks_linter"            
#>  [75] "paren_body_linter"                "paste_linter"                    
#>  [77] "pipe_call_linter"                 "pipe_consistency_linter"         
#>  [79] "pipe_continuation_linter"         "pipe_return_linter"              
#>  [81] "print_linter"                     "quotes_linter"                   
#>  [83] "redundant_equals_linter"          "redundant_ifelse_linter"         
#>  [85] "regex_subset_linter"              "rep_len_linter"                  
#>  [87] "repeat_linter"                    "return_linter"                   
#>  [89] "routine_registration_linter"      "sample_int_linter"               
#>  [91] "scalar_in_linter"                 "semicolon_linter"                
#>  [93] "seq_linter"                       "sort_linter"                     
#>  [95] "spaces_inside_linter"             "spaces_left_parentheses_linter"  
#>  [97] "sprintf_linter"                   "stopifnot_all_linter"            
#>  [99] "string_boundary_linter"           "strings_as_factors_linter"       
#> [101] "system_file_linter"               "T_and_F_symbol_linter"           
#> [103] "terminal_close_linter"            "todo_comment_linter"             
#> [105] "trailing_blank_lines_linter"      "trailing_whitespace_linter"      
#> [107] "undesirable_function_linter"      "undesirable_operator_linter"     
#> [109] "unnecessary_concatenation_linter" "unnecessary_lambda_linter"       
#> [111] "unnecessary_nesting_linter"       "unnecessary_placeholder_linter"  
#> [113] "unreachable_code_linter"          "unused_import_linter"            
#> [115] "vector_logic_linter"              "which_grepl_linter"              
#> [117] "whitespace_linter"                "yoda_test_linter"                
```
