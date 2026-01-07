#' Floating point comparison linter
#'
#' Comparing floating-point values for equality is fragile and can be affected
#'   by rounding error. Prefer checking whether the absolute difference is within
#'   an epsilon tolerance.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x == 3.0",
#'   linters = float_comparison_linter()
#' )
#'
#' lint(
#'   text = "y == 1/10",
#'   linters = float_comparison_linter()
#' )
#'
#' lint(
#'   text = "3.0 == x",
#'   linters = float_comparison_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x == 3L",
#'   linters = float_comparison_linter()
#' )
#'
#' @evalRd rd_tags("float_comparison_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @param lint_implicit_integer Logical, default `TRUE`. When `FALSE`, only
#'   non-integer numeric literals with a decimal point or exponent (e.g. `4.2`
#'   or `1e-3`) are linted; plain integer literals like `4` are ignored.
#' @export
float_comparison_linter <- function(lint_implicit_integer = TRUE) {
  integer_literal_filter <- if (lint_implicit_integer) {
    ""
  } else {
    "
      and (
        contains(text(), '.')
        or contains(text(), 'e')
        or contains(text(), 'E')
      )
    "
  }

  non_integer_const <- glue::glue("
    NUM_CONST[
      not(starts-with(text(), 'NA'))
      and not(text() = 'TRUE')
      and not(text() = 'FALSE')
      and not(text() = 'Inf')
      and not(substring(text(), string-length(text())) = 'L')
      {integer_literal_filter}
    ]
  ")

  integer_division <- "
    SPECIAL[text() = '%/%']
  "

  numeric_literal <- "
    NUM_CONST
    or (
      OP-MINUS
      and count(expr) = 1
      and expr[NUM_CONST]
    )
  "

  non_integer_division <- glue::glue("
    (OP-SLASH or OP-DIV)
    and count(expr) = 2
    and expr[1][{numeric_literal}]
    and expr[2][{numeric_literal}]
  ")

  base_non_integer_expr <- glue::glue("
    {non_integer_const}
    or (
      OP-MINUS
      and count(expr) = 1
      and expr[{non_integer_const}]
    )
    or (
      {non_integer_division}
    )
    or (
      OP-MINUS
      and count(expr) = 1
      and expr[{non_integer_division}]
    )
  ")

  non_integer_c_call <- glue::glue("
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'c']]
    and expr[{base_non_integer_expr}]
  ")

  non_integer_expr <- glue::glue("
    {base_non_integer_expr}
    or (
      {non_integer_c_call}
    )
  ")

  xpath <- glue::glue("
  (//EQ | //SPECIAL[text() = '%in%'])[
    parent::expr[
      expr[{non_integer_expr}]
      and not(expr[{integer_division}])
    ]
  ]
  | //expr[
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'match']]
    and expr[{non_integer_expr}]
  ]
  /expr[1]/SYMBOL_FUNCTION_CALL
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression,
      lint_message = "Avoid equality comparisons with non-integer numerics; use abs(x - y) < eps instead.",
      type = "warning"
    )
  })
}
