# Default linters

List of default linters for
[`lint()`](https://lintr.r-lib.org/reference/lint.md). Use
[`linters_with_defaults()`](https://lintr.r-lib.org/reference/linters_with_defaults.md)
to customize it. Most of the default linters are based on [the tidyverse
style guide](https://style.tidyverse.org/).

The set of default linters is as follows (any parameterized linters,
e.g., `line_length_linter` use their default argument(s), see
`?<linter_name>` for details):

## Usage

``` r
default_linters
```

## Format

An object of class `list` of length 26.

## See also

[linters](https://lintr.r-lib.org/reference/linters.md) for a complete
list of linters available in lintr.

## Linters

The following linters are tagged with 'default':

- [`assignment_linter`](https://lintr.r-lib.org/reference/assignment_linter.md)

- [`brace_linter`](https://lintr.r-lib.org/reference/brace_linter.md)

- [`commas_linter`](https://lintr.r-lib.org/reference/commas_linter.md)

- [`commented_code_linter`](https://lintr.r-lib.org/reference/commented_code_linter.md)

- [`equals_na_linter`](https://lintr.r-lib.org/reference/equals_na_linter.md)

- [`function_left_parentheses_linter`](https://lintr.r-lib.org/reference/function_left_parentheses_linter.md)

- [`indentation_linter`](https://lintr.r-lib.org/reference/indentation_linter.md)

- [`infix_spaces_linter`](https://lintr.r-lib.org/reference/infix_spaces_linter.md)

- [`line_length_linter`](https://lintr.r-lib.org/reference/line_length_linter.md)

- [`object_length_linter`](https://lintr.r-lib.org/reference/object_length_linter.md)

- [`object_name_linter`](https://lintr.r-lib.org/reference/object_name_linter.md)

- [`object_usage_linter`](https://lintr.r-lib.org/reference/object_usage_linter.md)

- [`paren_body_linter`](https://lintr.r-lib.org/reference/paren_body_linter.md)

- [`pipe_consistency_linter`](https://lintr.r-lib.org/reference/pipe_consistency_linter.md)

- [`pipe_continuation_linter`](https://lintr.r-lib.org/reference/pipe_continuation_linter.md)

- [`quotes_linter`](https://lintr.r-lib.org/reference/quotes_linter.md)

- [`return_linter`](https://lintr.r-lib.org/reference/return_linter.md)

- [`semicolon_linter`](https://lintr.r-lib.org/reference/semicolon_linter.md)

- [`seq_linter`](https://lintr.r-lib.org/reference/seq_linter.md)

- [`spaces_inside_linter`](https://lintr.r-lib.org/reference/spaces_inside_linter.md)

- [`spaces_left_parentheses_linter`](https://lintr.r-lib.org/reference/spaces_left_parentheses_linter.md)

- [`T_and_F_symbol_linter`](https://lintr.r-lib.org/reference/T_and_F_symbol_linter.md)

- [`trailing_blank_lines_linter`](https://lintr.r-lib.org/reference/trailing_blank_lines_linter.md)

- [`trailing_whitespace_linter`](https://lintr.r-lib.org/reference/trailing_whitespace_linter.md)

- [`vector_logic_linter`](https://lintr.r-lib.org/reference/vector_logic_linter.md)

- [`whitespace_linter`](https://lintr.r-lib.org/reference/whitespace_linter.md)
