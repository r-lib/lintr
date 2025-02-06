#' Require usage of boolean operators over equivalent arithmetic
#'
#' `length(which(x == y)) == 0` is the same as `!any(x == y)`, but the latter
#'   is more readable and more efficient.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "length(which(x == y)) == 0L",
#'   linters = boolean_arithmetic_linter()
#' )
#'
#' lint(
#'   text = "sum(grepl(pattern, x)) == 0",
#'   linters = boolean_arithmetic_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "!any(x == y)",
#'   linters = boolean_arithmetic_linter()
#' )
#'
#' lint(
#'   text = "!any(grepl(pattern, x))",
#'   linters = boolean_arithmetic_linter()
#' )
#'
#' @evalRd rd_tags("boolean_arithmetic_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
boolean_arithmetic_linter <- function() {
  # TODO(#1580): sum() cases x %in% y, A [&|] B, !A, is.na/is.nan/is.finite/is.infinite/is.element
  # TODO(#1581): extend to include all()-alike expressions
  zero_expr <- "(EQ or NE or GT or LE) and expr[NUM_CONST[text() = '0' or text() = '0L']]"
  one_expr <- "(LT or GE) and expr[NUM_CONST[text() = '1' or text() = '1L']]"
  length_xpath <- glue("
  parent::expr
    /parent::expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'length']]
      and parent::expr[ ({zero_expr}) or ({one_expr})]
    ]
  ")
  sum_xpath <- glue("
  parent::expr[
    expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'grepl']]
      or (EQ or NE or GT or LT or GE or LE)
    ]
    and parent::expr[ ({zero_expr}) or ({one_expr})]
  ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    length_calls <- source_expression$xml_find_function_calls(c("which", "grep"))
    sum_calls <- source_expression$xml_find_function_calls("sum")
    any_expr <- c(
      xml_find_all(length_calls, length_xpath),
      xml_find_all(sum_calls, sum_xpath)
    )

    xml_nodes_to_lints(
      any_expr,
      source_expression = source_expression,
      # TODO(#2464): customize this?
      lint_message = paste(
        "Use any() to express logical aggregations.",
        "For example, replace length(which(x == y)) == 0 with !any(x == y)."
      ),
      type = "warning"
    )
  })
}
