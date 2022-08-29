#' Require usage of `sort()` over `.[order(.)]`
#'
#' `sort()` is the dedicated option to sort a list or vector. It is more legible
#' and around twice as fast as `.[order(.)]`, with the gap in performance
#' growing with the vector size.
#'
#' @evalRd rd_tags("sort_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
sort_linter <- function() {
  xpath <- "//expr[
    OP-LEFT-BRACKET
    and expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'order']
        and following-sibling::expr =
          parent::expr
          /parent::expr
          /expr
      ]
    ]
  ]"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "sort(x) is better than x[order(x)].",
      type = "warning"
    )
  })
}
