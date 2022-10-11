#' Require usage of `sort()` over `.[order(.)]`
#'
#' `sort()` is the dedicated option to sort a list or vector. It is more legible
#' and around twice as fast as `.[order(.)]`, with the gap in performance
#' growing with the vector size.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x[order(x)]",
#'   linters = sort_linter()
#' )
#'
#' lint(
#'   text = "x[order(x, decreasing = TRUE)]",
#'   linters = sort_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x[sample(order(x))]",
#'   linters = sort_linter()
#' )
#'
#' lint(
#'   text = "y[order(x)]",
#'   linters = sort_linter()
#' )
#'
#' @evalRd rd_tags("sort_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
sort_linter <- function() {
  xpath <- "//OP-LEFT-BRACKET/following-sibling::expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'order']
        and following-sibling::expr =
          parent::expr
          /parent::expr
          /expr
      ]
    ]
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    var <- xml2::xml_text(xml2::xml_find_first(bad_expr, ".//SYMBOL"))

    msg <- glue::glue("sort({var}) is better than {var}[order({var})].")

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = msg,
      type = "warning"
    )
  })
}
