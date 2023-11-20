#' Return linter
#'
#' This linter checks for explicit [return()] at the end of a function
#'
#' @param use_implicit_returns Whether to use implicit or explicit returns
#'
#' @evalRd rd_tags("return_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#'
#' @export
return_linter <- function(use_implicit_returns = TRUE) {
  if (use_implicit_returns) {
    xpath <- "
      (//FUNCTION | //OP-LAMBDA)[following-sibling::expr[1]/*[1][self::OP-LEFT-BRACE]]
      /following-sibling::expr[1]/
      expr[last()][
        expr[1][
          not(OP-DOLLAR or OP-AT)
          and SYMBOL_FUNCTION_CALL[text() = 'return']
        ]
      ]
    "
    msg <- "Use implicit return behavior; explicit return() is not needed."
  } else {
    xpath <- "
      (//FUNCTION | //OP-LAMBDA)[following-sibling::expr[1]/*[1][self::OP-LEFT-BRACE]]
      /following-sibling::expr[1]/
      expr[last()][
        expr[1][not(
          (not(OP-DOLLAR or OP-AT) and SYMBOL_FUNCTION_CALL[text() = 'return' or text() = 'stop'])
        )] and
        expr[1][
          not(
            not(OP-DOLLAR or OP-AT) and SYMBOL_FUNCTION_CALL[text() = 'switch'] and
            not(following-sibling::expr[position()>1][
              not(descendant::SYMBOL_FUNCTION_CALL[text() = 'return' or text() = 'stop'])
            ]) and
            following-sibling::expr[position()>1][preceding-sibling::*[1][self::OP-COMMA]]
          )
        ] and
        not(
          REPEAT or IF[
            following-sibling::ELSE[
              following-sibling::expr[1][
                descendant::SYMBOL_FUNCTION_CALL[text() = 'return' or text() = 'stop']
              ]
            ]
          ]
        )
      ]
    "
    msg <- "All functions must have an explicit return()."
  }

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    xml_nodes <- xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      xml_nodes,
      source_expression = source_expression,
      lint_message = msg,
      type = "style"
    )
  })
}
