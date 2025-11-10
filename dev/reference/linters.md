# Available linters

A variety of linters are available in lintr. The most popular ones are
readily accessible through
[`default_linters()`](https://lintr.r-lib.org/dev/reference/default_linters.md).

Within a [`lint()`](https://lintr.r-lib.org/dev/reference/lint.md)
function call, the linters in use are initialized with the provided
arguments and fed with the source file (provided by
[`get_source_expressions()`](https://lintr.r-lib.org/dev/reference/get_source_expressions.md)).

A data frame of all available linters can be retrieved using
[`available_linters()`](https://lintr.r-lib.org/dev/reference/available_linters.md).
Documentation for linters is structured into tags to allow for easier
discovery; see also
[`available_tags()`](https://lintr.r-lib.org/dev/reference/available_linters.md).

## Tags

The following tags exist:

- [best_practices](https://lintr.r-lib.org/dev/reference/best_practices_linters.md)
  (65 linters)

- [common_mistakes](https://lintr.r-lib.org/dev/reference/common_mistakes_linters.md)
  (13 linters)

- [configurable](https://lintr.r-lib.org/dev/reference/configurable_linters.md)
  (44 linters)

- [consistency](https://lintr.r-lib.org/dev/reference/consistency_linters.md)
  (33 linters)

- [correctness](https://lintr.r-lib.org/dev/reference/correctness_linters.md)
  (7 linters)

- [default](https://lintr.r-lib.org/dev/reference/default_linters.md)
  (26 linters)

- [efficiency](https://lintr.r-lib.org/dev/reference/efficiency_linters.md)
  (30 linters)

- [executing](https://lintr.r-lib.org/dev/reference/executing_linters.md)
  (6 linters)

- [package_development](https://lintr.r-lib.org/dev/reference/package_development_linters.md)
  (14 linters)

- [pkg_testthat](https://lintr.r-lib.org/dev/reference/pkg_testthat_linters.md)
  (12 linters)

- [readability](https://lintr.r-lib.org/dev/reference/readability_linters.md)
  (66 linters)

- [regex](https://lintr.r-lib.org/dev/reference/regex_linters.md) (4
  linters)

- [robustness](https://lintr.r-lib.org/dev/reference/robustness_linters.md)
  (19 linters)

- [style](https://lintr.r-lib.org/dev/reference/style_linters.md) (40
  linters)

- [tidy_design](https://lintr.r-lib.org/dev/reference/tidy_design_linters.md)
  (1 linters)

## Linters

The following linters exist:

- [`absolute_path_linter`](https://lintr.r-lib.org/dev/reference/absolute_path_linter.md)
  (tags: best_practices, configurable, robustness)

- [`all_equal_linter`](https://lintr.r-lib.org/dev/reference/all_equal_linter.md)
  (tags: common_mistakes, robustness)

- [`any_duplicated_linter`](https://lintr.r-lib.org/dev/reference/any_duplicated_linter.md)
  (tags: best_practices, efficiency)

- [`any_is_na_linter`](https://lintr.r-lib.org/dev/reference/any_is_na_linter.md)
  (tags: best_practices, efficiency)

- [`assignment_linter`](https://lintr.r-lib.org/dev/reference/assignment_linter.md)
  (tags: configurable, consistency, default, style)

- [`backport_linter`](https://lintr.r-lib.org/dev/reference/backport_linter.md)
  (tags: configurable, package_development, robustness)

- [`boolean_arithmetic_linter`](https://lintr.r-lib.org/dev/reference/boolean_arithmetic_linter.md)
  (tags: best_practices, efficiency, readability)

- [`brace_linter`](https://lintr.r-lib.org/dev/reference/brace_linter.md)
  (tags: configurable, default, readability, style)

- [`class_equals_linter`](https://lintr.r-lib.org/dev/reference/class_equals_linter.md)
  (tags: best_practices, consistency, robustness)

- [`coalesce_linter`](https://lintr.r-lib.org/dev/reference/coalesce_linter.md)
  (tags: best_practices, consistency, readability)

- [`commas_linter`](https://lintr.r-lib.org/dev/reference/commas_linter.md)
  (tags: configurable, default, readability, style)

- [`commented_code_linter`](https://lintr.r-lib.org/dev/reference/commented_code_linter.md)
  (tags: best_practices, default, readability, style)

- [`comparison_negation_linter`](https://lintr.r-lib.org/dev/reference/comparison_negation_linter.md)
  (tags: consistency, readability)

- [`condition_call_linter`](https://lintr.r-lib.org/dev/reference/condition_call_linter.md)
  (tags: best_practices, configurable, style, tidy_design)

- [`condition_message_linter`](https://lintr.r-lib.org/dev/reference/condition_message_linter.md)
  (tags: best_practices, consistency)

- [`conjunct_test_linter`](https://lintr.r-lib.org/dev/reference/conjunct_test_linter.md)
  (tags: best_practices, configurable, package_development,
  pkg_testthat, readability)

- [`consecutive_assertion_linter`](https://lintr.r-lib.org/dev/reference/consecutive_assertion_linter.md)
  (tags: consistency, readability, style)

- [`consecutive_mutate_linter`](https://lintr.r-lib.org/dev/reference/consecutive_mutate_linter.md)
  (tags: configurable, consistency, efficiency, readability)

- [`cyclocomp_linter`](https://lintr.r-lib.org/dev/reference/cyclocomp_linter.md)
  (tags: best_practices, configurable, readability, style)

- [`download_file_linter`](https://lintr.r-lib.org/dev/reference/download_file_linter.md)
  (tags: best_practices, common_mistakes, robustness)

- [`duplicate_argument_linter`](https://lintr.r-lib.org/dev/reference/duplicate_argument_linter.md)
  (tags: common_mistakes, configurable, correctness)

- [`empty_assignment_linter`](https://lintr.r-lib.org/dev/reference/empty_assignment_linter.md)
  (tags: best_practices, readability)

- [`equals_na_linter`](https://lintr.r-lib.org/dev/reference/equals_na_linter.md)
  (tags: common_mistakes, correctness, default, robustness)

- [`expect_comparison_linter`](https://lintr.r-lib.org/dev/reference/expect_comparison_linter.md)
  (tags: best_practices, package_development, pkg_testthat)

- [`expect_identical_linter`](https://lintr.r-lib.org/dev/reference/expect_identical_linter.md)
  (tags: package_development, pkg_testthat)

- [`expect_length_linter`](https://lintr.r-lib.org/dev/reference/expect_length_linter.md)
  (tags: best_practices, package_development, pkg_testthat, readability)

- [`expect_named_linter`](https://lintr.r-lib.org/dev/reference/expect_named_linter.md)
  (tags: best_practices, package_development, pkg_testthat, readability)

- [`expect_not_linter`](https://lintr.r-lib.org/dev/reference/expect_not_linter.md)
  (tags: best_practices, package_development, pkg_testthat, readability)

- [`expect_null_linter`](https://lintr.r-lib.org/dev/reference/expect_null_linter.md)
  (tags: best_practices, package_development, pkg_testthat)

- [`expect_s3_class_linter`](https://lintr.r-lib.org/dev/reference/expect_s3_class_linter.md)
  (tags: best_practices, package_development, pkg_testthat)

- [`expect_s4_class_linter`](https://lintr.r-lib.org/dev/reference/expect_s4_class_linter.md)
  (tags: best_practices, package_development, pkg_testthat)

- [`expect_true_false_linter`](https://lintr.r-lib.org/dev/reference/expect_true_false_linter.md)
  (tags: best_practices, package_development, pkg_testthat, readability)

- [`expect_type_linter`](https://lintr.r-lib.org/dev/reference/expect_type_linter.md)
  (tags: best_practices, package_development, pkg_testthat)

- [`fixed_regex_linter`](https://lintr.r-lib.org/dev/reference/fixed_regex_linter.md)
  (tags: best_practices, configurable, efficiency, readability, regex)

- [`for_loop_index_linter`](https://lintr.r-lib.org/dev/reference/for_loop_index_linter.md)
  (tags: best_practices, readability, robustness)

- [`function_argument_linter`](https://lintr.r-lib.org/dev/reference/function_argument_linter.md)
  (tags: best_practices, consistency, style)

- [`function_left_parentheses_linter`](https://lintr.r-lib.org/dev/reference/function_left_parentheses_linter.md)
  (tags: default, readability, style)

- [`function_return_linter`](https://lintr.r-lib.org/dev/reference/function_return_linter.md)
  (tags: best_practices, readability)

- [`if_not_else_linter`](https://lintr.r-lib.org/dev/reference/if_not_else_linter.md)
  (tags: configurable, consistency, readability)

- [`if_switch_linter`](https://lintr.r-lib.org/dev/reference/if_switch_linter.md)
  (tags: best_practices, configurable, consistency, efficiency,
  readability)

- [`ifelse_censor_linter`](https://lintr.r-lib.org/dev/reference/ifelse_censor_linter.md)
  (tags: best_practices, efficiency)

- [`implicit_assignment_linter`](https://lintr.r-lib.org/dev/reference/implicit_assignment_linter.md)
  (tags: best_practices, configurable, readability, style)

- [`implicit_integer_linter`](https://lintr.r-lib.org/dev/reference/implicit_integer_linter.md)
  (tags: best_practices, configurable, consistency, style)

- [`indentation_linter`](https://lintr.r-lib.org/dev/reference/indentation_linter.md)
  (tags: configurable, default, readability, style)

- [`infix_spaces_linter`](https://lintr.r-lib.org/dev/reference/infix_spaces_linter.md)
  (tags: configurable, default, readability, style)

- [`inner_combine_linter`](https://lintr.r-lib.org/dev/reference/inner_combine_linter.md)
  (tags: consistency, efficiency, readability)

- [`is_numeric_linter`](https://lintr.r-lib.org/dev/reference/is_numeric_linter.md)
  (tags: best_practices, consistency, readability)

- [`keyword_quote_linter`](https://lintr.r-lib.org/dev/reference/keyword_quote_linter.md)
  (tags: consistency, readability, style)

- [`length_levels_linter`](https://lintr.r-lib.org/dev/reference/length_levels_linter.md)
  (tags: best_practices, consistency, readability)

- [`length_test_linter`](https://lintr.r-lib.org/dev/reference/length_test_linter.md)
  (tags: common_mistakes, efficiency)

- [`lengths_linter`](https://lintr.r-lib.org/dev/reference/lengths_linter.md)
  (tags: best_practices, efficiency, readability)

- [`library_call_linter`](https://lintr.r-lib.org/dev/reference/library_call_linter.md)
  (tags: best_practices, configurable, readability, style)

- [`line_length_linter`](https://lintr.r-lib.org/dev/reference/line_length_linter.md)
  (tags: configurable, default, readability, style)

- [`list2df_linter`](https://lintr.r-lib.org/dev/reference/list2df_linter.md)
  (tags: efficiency, readability)

- [`list_comparison_linter`](https://lintr.r-lib.org/dev/reference/list_comparison_linter.md)
  (tags: best_practices, common_mistakes)

- [`literal_coercion_linter`](https://lintr.r-lib.org/dev/reference/literal_coercion_linter.md)
  (tags: best_practices, consistency, efficiency)

- [`matrix_apply_linter`](https://lintr.r-lib.org/dev/reference/matrix_apply_linter.md)
  (tags: efficiency, readability)

- [`missing_argument_linter`](https://lintr.r-lib.org/dev/reference/missing_argument_linter.md)
  (tags: common_mistakes, configurable, correctness)

- [`missing_package_linter`](https://lintr.r-lib.org/dev/reference/missing_package_linter.md)
  (tags: common_mistakes, robustness)

- [`namespace_linter`](https://lintr.r-lib.org/dev/reference/namespace_linter.md)
  (tags: configurable, correctness, executing, robustness)

- [`nested_ifelse_linter`](https://lintr.r-lib.org/dev/reference/nested_ifelse_linter.md)
  (tags: efficiency, readability)

- [`nested_pipe_linter`](https://lintr.r-lib.org/dev/reference/nested_pipe_linter.md)
  (tags: configurable, consistency, readability)

- [`nonportable_path_linter`](https://lintr.r-lib.org/dev/reference/nonportable_path_linter.md)
  (tags: best_practices, configurable, robustness)

- [`nrow_subset_linter`](https://lintr.r-lib.org/dev/reference/nrow_subset_linter.md)
  (tags: best_practices, consistency, efficiency)

- [`numeric_leading_zero_linter`](https://lintr.r-lib.org/dev/reference/numeric_leading_zero_linter.md)
  (tags: consistency, readability, style)

- [`nzchar_linter`](https://lintr.r-lib.org/dev/reference/nzchar_linter.md)
  (tags: best_practices, consistency, efficiency)

- [`object_length_linter`](https://lintr.r-lib.org/dev/reference/object_length_linter.md)
  (tags: configurable, default, executing, readability, style)

- [`object_name_linter`](https://lintr.r-lib.org/dev/reference/object_name_linter.md)
  (tags: configurable, consistency, default, executing, style)

- [`object_overwrite_linter`](https://lintr.r-lib.org/dev/reference/object_overwrite_linter.md)
  (tags: best_practices, configurable, executing, readability,
  robustness)

- [`object_usage_linter`](https://lintr.r-lib.org/dev/reference/object_usage_linter.md)
  (tags: configurable, correctness, default, executing, readability,
  style)

- [`one_call_pipe_linter`](https://lintr.r-lib.org/dev/reference/one_call_pipe_linter.md)
  (tags: readability, style)

- [`outer_negation_linter`](https://lintr.r-lib.org/dev/reference/outer_negation_linter.md)
  (tags: best_practices, efficiency, readability)

- [`package_hooks_linter`](https://lintr.r-lib.org/dev/reference/package_hooks_linter.md)
  (tags: correctness, package_development, style)

- [`paren_body_linter`](https://lintr.r-lib.org/dev/reference/paren_body_linter.md)
  (tags: default, readability, style)

- [`paste_linter`](https://lintr.r-lib.org/dev/reference/paste_linter.md)
  (tags: best_practices, configurable, consistency)

- [`pipe_call_linter`](https://lintr.r-lib.org/dev/reference/pipe_call_linter.md)
  (tags: readability, style)

- [`pipe_consistency_linter`](https://lintr.r-lib.org/dev/reference/pipe_consistency_linter.md)
  (tags: configurable, default, readability, style)

- [`pipe_continuation_linter`](https://lintr.r-lib.org/dev/reference/pipe_continuation_linter.md)
  (tags: default, readability, style)

- [`pipe_return_linter`](https://lintr.r-lib.org/dev/reference/pipe_return_linter.md)
  (tags: best_practices, common_mistakes)

- [`print_linter`](https://lintr.r-lib.org/dev/reference/print_linter.md)
  (tags: best_practices, consistency)

- [`quotes_linter`](https://lintr.r-lib.org/dev/reference/quotes_linter.md)
  (tags: configurable, consistency, default, readability, style)

- [`redundant_equals_linter`](https://lintr.r-lib.org/dev/reference/redundant_equals_linter.md)
  (tags: best_practices, common_mistakes, efficiency, readability)

- [`redundant_ifelse_linter`](https://lintr.r-lib.org/dev/reference/redundant_ifelse_linter.md)
  (tags: best_practices, configurable, consistency, efficiency)

- [`regex_subset_linter`](https://lintr.r-lib.org/dev/reference/regex_subset_linter.md)
  (tags: best_practices, efficiency, regex)

- [`rep_len_linter`](https://lintr.r-lib.org/dev/reference/rep_len_linter.md)
  (tags: best_practices, consistency, readability)

- [`repeat_linter`](https://lintr.r-lib.org/dev/reference/repeat_linter.md)
  (tags: readability, style)

- [`return_linter`](https://lintr.r-lib.org/dev/reference/return_linter.md)
  (tags: configurable, default, style)

- [`routine_registration_linter`](https://lintr.r-lib.org/dev/reference/routine_registration_linter.md)
  (tags: best_practices, efficiency, robustness)

- [`sample_int_linter`](https://lintr.r-lib.org/dev/reference/sample_int_linter.md)
  (tags: efficiency, readability, robustness)

- [`scalar_in_linter`](https://lintr.r-lib.org/dev/reference/scalar_in_linter.md)
  (tags: best_practices, configurable, consistency, efficiency,
  readability)

- [`semicolon_linter`](https://lintr.r-lib.org/dev/reference/semicolon_linter.md)
  (tags: configurable, default, readability, style)

- [`seq_linter`](https://lintr.r-lib.org/dev/reference/seq_linter.md)
  (tags: best_practices, consistency, default, efficiency, robustness)

- [`sort_linter`](https://lintr.r-lib.org/dev/reference/sort_linter.md)
  (tags: best_practices, efficiency, readability)

- [`spaces_inside_linter`](https://lintr.r-lib.org/dev/reference/spaces_inside_linter.md)
  (tags: default, readability, style)

- [`spaces_left_parentheses_linter`](https://lintr.r-lib.org/dev/reference/spaces_left_parentheses_linter.md)
  (tags: default, readability, style)

- [`sprintf_linter`](https://lintr.r-lib.org/dev/reference/sprintf_linter.md)
  (tags: common_mistakes, correctness)

- [`stopifnot_all_linter`](https://lintr.r-lib.org/dev/reference/stopifnot_all_linter.md)
  (tags: best_practices, readability)

- [`string_boundary_linter`](https://lintr.r-lib.org/dev/reference/string_boundary_linter.md)
  (tags: configurable, efficiency, readability, regex)

- [`strings_as_factors_linter`](https://lintr.r-lib.org/dev/reference/strings_as_factors_linter.md)
  (tags: robustness)

- [`system_file_linter`](https://lintr.r-lib.org/dev/reference/system_file_linter.md)
  (tags: best_practices, consistency, readability)

- [`T_and_F_symbol_linter`](https://lintr.r-lib.org/dev/reference/T_and_F_symbol_linter.md)
  (tags: best_practices, consistency, default, readability, robustness,
  style)

- [`terminal_close_linter`](https://lintr.r-lib.org/dev/reference/terminal_close_linter.md)
  (tags: best_practices, robustness)

- [`todo_comment_linter`](https://lintr.r-lib.org/dev/reference/todo_comment_linter.md)
  (tags: configurable, style)

- [`trailing_blank_lines_linter`](https://lintr.r-lib.org/dev/reference/trailing_blank_lines_linter.md)
  (tags: default, style)

- [`trailing_whitespace_linter`](https://lintr.r-lib.org/dev/reference/trailing_whitespace_linter.md)
  (tags: configurable, default, style)

- [`undesirable_function_linter`](https://lintr.r-lib.org/dev/reference/undesirable_function_linter.md)
  (tags: best_practices, configurable, robustness, style)

- [`undesirable_operator_linter`](https://lintr.r-lib.org/dev/reference/undesirable_operator_linter.md)
  (tags: best_practices, configurable, robustness, style)

- [`unnecessary_concatenation_linter`](https://lintr.r-lib.org/dev/reference/unnecessary_concatenation_linter.md)
  (tags: configurable, efficiency, readability, style)

- [`unnecessary_lambda_linter`](https://lintr.r-lib.org/dev/reference/unnecessary_lambda_linter.md)
  (tags: best_practices, configurable, efficiency, readability)

- [`unnecessary_nesting_linter`](https://lintr.r-lib.org/dev/reference/unnecessary_nesting_linter.md)
  (tags: best_practices, configurable, consistency, readability)

- [`unnecessary_placeholder_linter`](https://lintr.r-lib.org/dev/reference/unnecessary_placeholder_linter.md)
  (tags: best_practices, readability)

- [`unreachable_code_linter`](https://lintr.r-lib.org/dev/reference/unreachable_code_linter.md)
  (tags: best_practices, configurable, readability)

- [`unused_import_linter`](https://lintr.r-lib.org/dev/reference/unused_import_linter.md)
  (tags: best_practices, common_mistakes, configurable, executing)

- [`vector_logic_linter`](https://lintr.r-lib.org/dev/reference/vector_logic_linter.md)
  (tags: best_practices, common_mistakes, default, efficiency)

- [`which_grepl_linter`](https://lintr.r-lib.org/dev/reference/which_grepl_linter.md)
  (tags: consistency, efficiency, readability, regex)

- [`whitespace_linter`](https://lintr.r-lib.org/dev/reference/whitespace_linter.md)
  (tags: consistency, default, style)

- [`yoda_test_linter`](https://lintr.r-lib.org/dev/reference/yoda_test_linter.md)
  (tags: best_practices, package_development, pkg_testthat, readability)
