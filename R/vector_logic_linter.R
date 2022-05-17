#' Enforce usage of scalar logical operators in conditional statements
#'
#' Usage of `&` in conditional statements is error-prone and inefficient.
#'   `condition` in `if (condition) expr` must always be length-1, in which
#'   case `&&` is to be preferred. Ditto for `|` vs. `||`.
#'
#' This linter covers inputs to `if()` and `while()` conditions and to
#'   [testthat::expect_true()] and [testthat::expect_false()].
#' @evalRd rd_tags("vector_logic_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#if-statements>
#' @export
vector_logic_linter <- function() {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression", require_xml = TRUE)) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    # ensures the expr is in the cond part of `if/while (cond) expr` --
    #   if on the XML parse tree is structured like
    #   <expr>
    #     <IF> | <expr><SYMBOL_FUNCTION_CALL>
    #     <OP-LEFT-PAREN>
    #     <expr> ... </expr> # <- loop condition
    #     <OP-RIGHT-PAREN>
    #     <expr> ... </expr> # <- evaluation; includes BRACEs if present
    #     <ELSE>             # (here & below is optional)
    #     <expr> ... </expr>
    #  </expr>
    #  we _don't_ want to match anything on the second expr, hence this
    xpath <- "//*[
      (self::AND or self::OR)
      and ancestor::expr[
        not(preceding-sibling::OP-RIGHT-PAREN)
        and preceding-sibling::*[
          self::IF
          or self::WHILE
          or self::expr[SYMBOL_FUNCTION_CALL[text() = 'expect_true' or text() = 'expect_false']]
        ]
      ]
      and not(ancestor::expr[
        preceding-sibling::expr[SYMBOL_FUNCTION_CALL[not(text() = 'expect_true' or text() = 'expect_false')]]
        or preceding-sibling::OP-LEFT-BRACKET
      ])
    ]"
    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lint(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Conditional expressions require scalar logical operators (&& and ||)",
      type = "warning"
    )
  })
}
