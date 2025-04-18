#' Require usage of `!any(x)` over `all(!x)`, `!all(x)` over `any(!x)`
#'
#' `any(!x)` is logically equivalent to `!all(x)`; ditto for the equivalence of
#'   `all(!x)` and `!any(x)`. Negating after aggregation only requires inverting
#'   one logical value, and is typically more readable.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "all(!x)",
#'   linters = outer_negation_linter()
#' )
#'
#' lint(
#'   text = "any(!x)",
#'   linters = outer_negation_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "!any(x)",
#'   linters = outer_negation_linter()
#' )
#'
#' lint(
#'   text = "!all(x)",
#'   linters = outer_negation_linter()
#' )
#'
#' @evalRd rd_tags("outer_negation_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
outer_negation_linter <- function() {
  # NB: the double negation is a bit hairy, but it's what we need to check if
  #   _all_ of the inputs to any(..., na.rm=na.rm) are negated, i.e., there are
  #   _not_ any entries that are _not_ negated. IINM that's what we're stuck
  #   with in xpath if we want to guarantee a condition on _all_ <expr>
  #   coming after any( and before na.rm= .
  # NB: requirement that count(expr)>1 is to prevent any() from linting
  #   e.g. in magrittr pipelines.
  xpath <- "
  self::*[following-sibling::expr]
    /parent::expr[
      not(expr[
        position() > 1
        and not(OP-EXCLAMATION)
        and not(preceding-sibling::*[1][self::EQ_SUB])
      ])
    ]
  "

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(c("any", "all"))
    bad_expr <- xml_find_all(xml_calls, xpath)

    matched_call <- xp_call_name(bad_expr)
    inverse_call <- ifelse(matched_call == "any", "all", "any")
    lint_message <- paste(
      sprintf("!%s(x) is better than %s(!x).", inverse_call, matched_call),
      "The former applies negation only once after aggregation instead of many times for each element of x."
    )

    xml_nodes_to_lints(bad_expr, source_expression, lint_message, type = "warning")
  })
}
