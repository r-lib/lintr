# Generated Rd fragments are as expected for rd_tags

    Code
      rd_tags("assignment_linter")
    Output
      [1] "\\section{Tags}{"                                                                                           
      [2] "\\link[=consistency_linters]{consistency}, \\link[=default_linters]{default}, \\link[=style_linters]{style}"
      [3] "}"                                                                                                          

# Generated Rd fragments are as expected for rd_linters

    Code
      rd_linters("robustness")
    Output
       [1] "\\section{Linters}{"                                
       [2] "The following linters are tagged with 'robustness':"
       [3] "\\itemize{"                                         
       [4] "\\item{\\code{\\link{absolute_path_linter}}}"       
       [5] "\\item{\\code{\\link{backport_linter}}}"            
       [6] "\\item{\\code{\\link{class_equals_linter}}}"        
       [7] "\\item{\\code{\\link{equals_na_linter}}}"           
       [8] "\\item{\\code{\\link{missing_package_linter}}}"     
       [9] "\\item{\\code{\\link{namespace_linter}}}"           
      [10] "\\item{\\code{\\link{nonportable_path_linter}}}"    
      [11] "\\item{\\code{\\link{seq_linter}}}"                 
      [12] "\\item{\\code{\\link{strings_as_factors_linter}}}"  
      [13] "\\item{\\code{\\link{T_and_F_symbol_linter}}}"      
      [14] "\\item{\\code{\\link{undesirable_function_linter}}}"
      [15] "\\item{\\code{\\link{undesirable_operator_linter}}}"
      [16] "}"                                                  
      [17] "}"                                                  

# Generated Rd fragments are as expected for rd_taglist

    Code
      rd_taglist()
    Output
                                                                                       
                                                                    "\\section{Tags}{" 
                                                                                       
                                                           "The following tags exist:" 
                                                                                       
                                                                          "\\itemize{" 
                                                                        best_practices 
                "\\item{\\link[=best_practices_linters]{best_practices} (42 linters)}" 
                                                                       common_mistakes 
               "\\item{\\link[=common_mistakes_linters]{common_mistakes} (7 linters)}" 
                                                                          configurable 
                    "\\item{\\link[=configurable_linters]{configurable} (20 linters)}" 
                                                                           consistency 
                      "\\item{\\link[=consistency_linters]{consistency} (17 linters)}" 
                                                                           correctness 
                       "\\item{\\link[=correctness_linters]{correctness} (7 linters)}" 
                                                                               default 
                              "\\item{\\link[=default_linters]{default} (24 linters)}" 
                                                                            deprecated 
                         "\\item{\\link[=deprecated_linters]{deprecated} (4 linters)}" 
                                                                            efficiency 
                        "\\item{\\link[=efficiency_linters]{efficiency} (20 linters)}" 
                                                                             executing 
                           "\\item{\\link[=executing_linters]{executing} (5 linters)}" 
                                                                   package_development 
      "\\item{\\link[=package_development_linters]{package_development} (14 linters)}" 
                                                                           readability 
                      "\\item{\\link[=readability_linters]{readability} (42 linters)}" 
                                                                            robustness 
                        "\\item{\\link[=robustness_linters]{robustness} (12 linters)}" 
                                                                                 style 
                                  "\\item{\\link[=style_linters]{style} (36 linters)}" 
                                                                                       
                                                                                   "}" 
                                                                                       
                                                                                   "}" 

# Generated Rd fragments are as expected for rd_linterlist

    Code
      rd_linterlist()
    Output
                                                                                                                                   
                                                                                                             "\\section{Linters}{" 
                                                                                                                                   
                                                                                                    "The following linters exist:" 
                                                                                                                                   
                                                                                                                      "\\itemize{" 
                                                                                                              absolute_path_linter 
                                   "\\item{\\code{\\link{absolute_path_linter}} (tags: best_practices, configurable, robustness)}" 
                                                                                                             any_duplicated_linter 
                                                "\\item{\\code{\\link{any_duplicated_linter}} (tags: best_practices, efficiency)}" 
                                                                                                                  any_is_na_linter 
                                                     "\\item{\\code{\\link{any_is_na_linter}} (tags: best_practices, efficiency)}" 
                                                                                                                 assignment_linter 
                                                   "\\item{\\code{\\link{assignment_linter}} (tags: consistency, default, style)}" 
                                                                                                                   backport_linter 
                                   "\\item{\\code{\\link{backport_linter}} (tags: configurable, package_development, robustness)}" 
                                                                                                         boolean_arithmetic_linter 
                               "\\item{\\code{\\link{boolean_arithmetic_linter}} (tags: best_practices, efficiency, readability)}" 
                                                                                                                      brace_linter 
                                          "\\item{\\code{\\link{brace_linter}} (tags: configurable, default, readability, style)}" 
                                                                                                               class_equals_linter 
                                     "\\item{\\code{\\link{class_equals_linter}} (tags: best_practices, consistency, robustness)}" 
                                                                                                               closed_curly_linter 
                                "\\item{\\code{\\link{closed_curly_linter}} (tags: configurable, deprecated, readability, style)}" 
                                                                                                                     commas_linter 
                                                       "\\item{\\code{\\link{commas_linter}} (tags: default, readability, style)}" 
                                                                                                             commented_code_linter 
                               "\\item{\\code{\\link{commented_code_linter}} (tags: best_practices, default, readability, style)}" 
                                                                                                          condition_message_linter 
                                            "\\item{\\code{\\link{condition_message_linter}} (tags: best_practices, consistency)}" 
                                                                                                              conjunct_test_linter 
                           "\\item{\\code{\\link{conjunct_test_linter}} (tags: best_practices, package_development, readability)}" 
                                                                                                      consecutive_stopifnot_linter 
                                    "\\item{\\code{\\link{consecutive_stopifnot_linter}} (tags: consistency, readability, style)}" 
                                                                                                                  cyclocomp_linter 
                      "\\item{\\code{\\link{cyclocomp_linter}} (tags: best_practices, configurable, default, readability, style)}" 
                                                                                                         duplicate_argument_linter 
                            "\\item{\\code{\\link{duplicate_argument_linter}} (tags: common_mistakes, configurable, correctness)}" 
                                                                                                                  equals_na_linter 
                              "\\item{\\code{\\link{equals_na_linter}} (tags: common_mistakes, correctness, default, robustness)}" 
                                                                                                          expect_comparison_linter 
                                    "\\item{\\code{\\link{expect_comparison_linter}} (tags: best_practices, package_development)}" 
                                                                                                           expect_identical_linter 
                                                     "\\item{\\code{\\link{expect_identical_linter}} (tags: package_development)}" 
                                                                                                              expect_length_linter 
                           "\\item{\\code{\\link{expect_length_linter}} (tags: best_practices, package_development, readability)}" 
                                                                                                               expect_named_linter 
                            "\\item{\\code{\\link{expect_named_linter}} (tags: best_practices, package_development, readability)}" 
                                                                                                                 expect_not_linter 
                              "\\item{\\code{\\link{expect_not_linter}} (tags: best_practices, package_development, readability)}" 
                                                                                                                expect_null_linter 
                                          "\\item{\\code{\\link{expect_null_linter}} (tags: best_practices, package_development)}" 
                                                                                                            expect_s3_class_linter 
                                      "\\item{\\code{\\link{expect_s3_class_linter}} (tags: best_practices, package_development)}" 
                                                                                                            expect_s4_class_linter 
                                      "\\item{\\code{\\link{expect_s4_class_linter}} (tags: best_practices, package_development)}" 
                                                                                                          expect_true_false_linter 
                       "\\item{\\code{\\link{expect_true_false_linter}} (tags: best_practices, package_development, readability)}" 
                                                                                                                expect_type_linter 
                                          "\\item{\\code{\\link{expect_type_linter}} (tags: best_practices, package_development)}" 
                                                                                                        extraction_operator_linter 
                                                "\\item{\\code{\\link{extraction_operator_linter}} (tags: best_practices, style)}" 
                                                                                                                fixed_regex_linter 
                                      "\\item{\\code{\\link{fixed_regex_linter}} (tags: best_practices, efficiency, readability)}" 
                                                                                                          function_argument_linter 
                                     "\\item{\\code{\\link{function_argument_linter}} (tags: best_practices, consistency, style)}" 
                                                                                                  function_left_parentheses_linter 
                                    "\\item{\\code{\\link{function_left_parentheses_linter}} (tags: default, readability, style)}" 
                                                                                                            function_return_linter 
                                              "\\item{\\code{\\link{function_return_linter}} (tags: best_practices, readability)}" 
                                                                                                              ifelse_censor_linter 
                                                 "\\item{\\code{\\link{ifelse_censor_linter}} (tags: best_practices, efficiency)}" 
                                                                                                           implicit_integer_linter 
                                      "\\item{\\code{\\link{implicit_integer_linter}} (tags: best_practices, consistency, style)}" 
                                                                                                               infix_spaces_linter 
                                                 "\\item{\\code{\\link{infix_spaces_linter}} (tags: default, readability, style)}" 
                                                                                                              inner_combine_linter 
                                       "\\item{\\code{\\link{inner_combine_linter}} (tags: consistency, efficiency, readability)}" 
                                                                                                                    lengths_linter 
                                          "\\item{\\code{\\link{lengths_linter}} (tags: best_practices, efficiency, readability)}" 
                                                                                                                line_length_linter 
                                    "\\item{\\code{\\link{line_length_linter}} (tags: configurable, default, readability, style)}" 
                                                                                                           literal_coercion_linter 
                                 "\\item{\\code{\\link{literal_coercion_linter}} (tags: best_practices, consistency, efficiency)}" 
                                                                                                           missing_argument_linter 
                              "\\item{\\code{\\link{missing_argument_linter}} (tags: common_mistakes, configurable, correctness)}" 
                                                                                                            missing_package_linter 
                                              "\\item{\\code{\\link{missing_package_linter}} (tags: common_mistakes, robustness)}" 
                                                                                                                  namespace_linter 
                               "\\item{\\code{\\link{namespace_linter}} (tags: configurable, correctness, executing, robustness)}" 
                                                                                                              nested_ifelse_linter 
                                                    "\\item{\\code{\\link{nested_ifelse_linter}} (tags: efficiency, readability)}" 
                                                                                                                     no_tab_linter 
                                                       "\\item{\\code{\\link{no_tab_linter}} (tags: consistency, default, style)}" 
                                                                                                           nonportable_path_linter 
                                "\\item{\\code{\\link{nonportable_path_linter}} (tags: best_practices, configurable, robustness)}" 
                                                                                                       numeric_leading_zero_linter 
                                     "\\item{\\code{\\link{numeric_leading_zero_linter}} (tags: consistency, readability, style)}" 
                                                                                                              object_length_linter 
                       "\\item{\\code{\\link{object_length_linter}} (tags: configurable, default, executing, readability, style)}" 
                                                                                                                object_name_linter 
                         "\\item{\\code{\\link{object_name_linter}} (tags: configurable, consistency, default, executing, style)}" 
                                                                                                               object_usage_linter 
                         "\\item{\\code{\\link{object_usage_linter}} (tags: correctness, default, executing, readability, style)}" 
                                                                                                                 open_curly_linter 
                                  "\\item{\\code{\\link{open_curly_linter}} (tags: configurable, deprecated, readability, style)}" 
                                                                                                             outer_negation_linter 
                                   "\\item{\\code{\\link{outer_negation_linter}} (tags: best_practices, efficiency, readability)}" 
                                                                                                              package_hooks_linter 
                                    "\\item{\\code{\\link{package_hooks_linter}} (tags: correctness, package_development, style)}" 
                                                                                                                 paren_body_linter 
                                                   "\\item{\\code{\\link{paren_body_linter}} (tags: default, readability, style)}" 
                                                                                                                paren_brace_linter 
                                               "\\item{\\code{\\link{paren_brace_linter}} (tags: deprecated, readability, style)}" 
                                                                                                                      paste_linter 
                                                        "\\item{\\code{\\link{paste_linter}} (tags: best_practices, consistency)}" 
                                                                                                                  pipe_call_linter 
                                                             "\\item{\\code{\\link{pipe_call_linter}} (tags: readability, style)}" 
                                                                                                          pipe_continuation_linter 
                                            "\\item{\\code{\\link{pipe_continuation_linter}} (tags: default, readability, style)}" 
                                                                                                           redundant_equals_linter 
                "\\item{\\code{\\link{redundant_equals_linter}} (tags: best_practices, common_mistakes, efficiency, readability)}" 
                                                                                                           redundant_ifelse_linter 
                                 "\\item{\\code{\\link{redundant_ifelse_linter}} (tags: best_practices, consistency, efficiency)}" 
                                                                                                               regex_subset_linter 
                                                  "\\item{\\code{\\link{regex_subset_linter}} (tags: best_practices, efficiency)}" 
                                                                                                                  semicolon_linter 
                                      "\\item{\\code{\\link{semicolon_linter}} (tags: configurable, default, readability, style)}" 
                                                                                                       semicolon_terminator_linter 
                        "\\item{\\code{\\link{semicolon_terminator_linter}} (tags: configurable, deprecated, readability, style)}" 
                                                                                                                        seq_linter 
                         "\\item{\\code{\\link{seq_linter}} (tags: best_practices, consistency, default, efficiency, robustness)}" 
                                                                                                              single_quotes_linter 
                                   "\\item{\\code{\\link{single_quotes_linter}} (tags: consistency, default, readability, style)}" 
                                                                                                              spaces_inside_linter 
                                                "\\item{\\code{\\link{spaces_inside_linter}} (tags: default, readability, style)}" 
                                                                                                    spaces_left_parentheses_linter 
                                      "\\item{\\code{\\link{spaces_left_parentheses_linter}} (tags: default, readability, style)}" 
                                                                                                                    sprintf_linter 
                                                     "\\item{\\code{\\link{sprintf_linter}} (tags: common_mistakes, correctness)}" 
                                                                                                            string_boundary_linter 
                                                  "\\item{\\code{\\link{string_boundary_linter}} (tags: efficiency, readability)}" 
                                                                                                         strings_as_factors_linter 
                                                            "\\item{\\code{\\link{strings_as_factors_linter}} (tags: robustness)}" 
                                                                                                                system_file_linter 
                                     "\\item{\\code{\\link{system_file_linter}} (tags: best_practices, consistency, readability)}" 
                                                                                                             T_and_F_symbol_linter 
      "\\item{\\code{\\link{T_and_F_symbol_linter}} (tags: best_practices, consistency, default, readability, robustness, style)}" 
                                                                                                               todo_comment_linter 
                                                         "\\item{\\code{\\link{todo_comment_linter}} (tags: configurable, style)}" 
                                                                                                       trailing_blank_lines_linter 
                                                      "\\item{\\code{\\link{trailing_blank_lines_linter}} (tags: default, style)}" 
                                                                                                        trailing_whitespace_linter 
                                                       "\\item{\\code{\\link{trailing_whitespace_linter}} (tags: default, style)}" 
                                                                                                       undesirable_function_linter 
         "\\item{\\code{\\link{undesirable_function_linter}} (tags: best_practices, configurable, efficiency, robustness, style)}" 
                                                                                                       undesirable_operator_linter 
         "\\item{\\code{\\link{undesirable_operator_linter}} (tags: best_practices, configurable, efficiency, robustness, style)}" 
                                                                                                         unnecessary_lambda_linter 
                               "\\item{\\code{\\link{unnecessary_lambda_linter}} (tags: best_practices, efficiency, readability)}" 
                                                                                                     unneeded_concatenation_linter 
                      "\\item{\\code{\\link{unneeded_concatenation_linter}} (tags: configurable, efficiency, readability, style)}" 
                                                                                                           unreachable_code_linter 
                                             "\\item{\\code{\\link{unreachable_code_linter}} (tags: best_practices, readability)}" 
                                                                                                              unused_import_linter 
                   "\\item{\\code{\\link{unused_import_linter}} (tags: best_practices, common_mistakes, configurable, executing)}" 
                                                                                                               vector_logic_linter 
                                         "\\item{\\code{\\link{vector_logic_linter}} (tags: best_practices, default, efficiency)}" 
                                                                                                                  yoda_test_linter 
                               "\\item{\\code{\\link{yoda_test_linter}} (tags: best_practices, package_development, readability)}" 
                                                                                                                                   
                                                                                                                               "}" 
                                                                                                                                   
                                                                                                                               "}" 

