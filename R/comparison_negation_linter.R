#' Block usages like !(x == y) where a direct relational operator is appropriate
#'
#' `!(x == y)` is more readably expressed as `x != y`. The same is true of
#'   other negations of simple comparisons like `!(x > y)` and `!(x <= y)`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "!x == 2",
#'   linters = comparison_negation_linter()
#' )
#'
#' lint(
#'   text = "!(x > 2)",
#'   linters = comparison_negation_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "!(x == 2 & y > 2)",
#'   linters = comparison_negation_linter()
#' )
#'
#' lint(
#'   text = "!(x & y)",
#'   linters = comparison_negation_linter()
#' )
#'
#' lint(
#'   text = "x != 2",
#'   linters = comparison_negation_linter()
#' )
#'
#' @evalRd rd_tags("comparison_negation_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
comparison_negation_linter <- function() {
  comparators <- infix_metadata$xml_tag[infix_metadata$comparator]

  comparator_inverses <- c(EQ = "!=", NE = "==", GE = "<", LE = ">", GT = "<=", LT = ">=")

  # outline
  #   - match !
  #   - avoid false positives like `!!x > y`
  #     + two conditions: one for the "outer" ! and one for the "inner" !
  #   - avoid false positives like !any(x > y)
  #   - avoid false positives like !x[f == g]
  #   - make sure to catch both !(x == y) and !x == y
  xpath <- glue("
  //OP-EXCLAMATION
    /parent::expr[
      not(parent::expr[OP-EXCLAMATION])
      and expr[
        not(expr[SYMBOL_FUNCTION_CALL] or OP-EXCLAMATION or OP-LEFT-BRACKET)
        and (
          expr[ {xp_or(comparators)} ]
          or {xp_or(comparators)}
        )
      ]
    ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    comparator_node <- xml_find_first(bad_expr, "expr/expr/*[2]")
    comparator_name <- xml_name(comparator_node)

    # "typical" case is assumed to be !(x == y), so try that first, and back
    #   up to the less nested case. there may be a cleaner way to do this...
    unnested <- !comparator_name %in% names(comparator_inverses)
    comparator_node[unnested] <- xml_find_first(bad_expr[unnested], "expr/*[2]")
    comparator_name[unnested] <- xml_name(comparator_node[unnested])

    comparator_text <- xml_text(comparator_node)
    inverse <- comparator_inverses[comparator_name]

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = glue("Use x {inverse} y, not !(x {comparator_text} y)."),
      type = "warning"
    )
  })
}
