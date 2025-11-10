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
#>  [31] "expect_s4_class_linter"           "expect_true_false_linter"        
#>  [33] "expect_type_linter"               "fixed_regex_linter"              
#>  [35] "for_loop_index_linter"            "function_argument_linter"        
#>  [37] "function_left_parentheses_linter" "function_return_linter"          
#>  [39] "if_not_else_linter"               "if_switch_linter"                
#>  [41] "ifelse_censor_linter"             "implicit_assignment_linter"      
#>  [43] "implicit_integer_linter"          "indentation_linter"              
#>  [45] "infix_spaces_linter"              "inner_combine_linter"            
#>  [47] "is_numeric_linter"                "keyword_quote_linter"            
#>  [49] "length_levels_linter"             "length_test_linter"              
#>  [51] "lengths_linter"                   "library_call_linter"             
#>  [53] "line_length_linter"               "list2df_linter"                  
#>  [55] "list_comparison_linter"           "literal_coercion_linter"         
#>  [57] "matrix_apply_linter"              "missing_argument_linter"         
#>  [59] "missing_package_linter"           "namespace_linter"                
#>  [61] "nested_ifelse_linter"             "nested_pipe_linter"              
#>  [63] "nonportable_path_linter"          "nrow_subset_linter"              
#>  [65] "numeric_leading_zero_linter"      "nzchar_linter"                   
#>  [67] "object_length_linter"             "object_name_linter"              
#>  [69] "object_overwrite_linter"          "object_usage_linter"             
#>  [71] "one_call_pipe_linter"             "outer_negation_linter"           
#>  [73] "package_hooks_linter"             "paren_body_linter"               
#>  [75] "paste_linter"                     "pipe_call_linter"                
#>  [77] "pipe_consistency_linter"          "pipe_continuation_linter"        
#>  [79] "pipe_return_linter"               "print_linter"                    
#>  [81] "quotes_linter"                    "redundant_equals_linter"         
#>  [83] "redundant_ifelse_linter"          "regex_subset_linter"             
#>  [85] "rep_len_linter"                   "repeat_linter"                   
#>  [87] "return_linter"                    "routine_registration_linter"     
#>  [89] "sample_int_linter"                "scalar_in_linter"                
#>  [91] "semicolon_linter"                 "seq_linter"                      
#>  [93] "sort_linter"                      "spaces_inside_linter"            
#>  [95] "spaces_left_parentheses_linter"   "sprintf_linter"                  
#>  [97] "stopifnot_all_linter"             "string_boundary_linter"          
#>  [99] "strings_as_factors_linter"        "system_file_linter"              
#> [101] "T_and_F_symbol_linter"            "terminal_close_linter"           
#> [103] "todo_comment_linter"              "trailing_blank_lines_linter"     
#> [105] "trailing_whitespace_linter"       "undesirable_function_linter"     
#> [107] "undesirable_operator_linter"      "unnecessary_concatenation_linter"
#> [109] "unnecessary_lambda_linter"        "unnecessary_nesting_linter"      
#> [111] "unnecessary_placeholder_linter"   "unreachable_code_linter"         
#> [113] "unused_import_linter"             "vector_logic_linter"             
#> [115] "which_grepl_linter"               "whitespace_linter"               
#> [117] "yoda_test_linter"                
```
