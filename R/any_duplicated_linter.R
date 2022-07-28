#' Require usage of `anyDuplicated(x) > 0` over `any(duplicated(x))`
#'
#' [anyDuplicated()] exists as a replacement for `any(duplicated(.))`, which is
#'   more efficient for simple objects, and is equally efficient in the worst case scenario.
#'   Therefore, it should be used in all situations instead of the latter.
#'
#' Also match usage like `length(unique(x$col)) == nrow(x)`, which can
#'   be replaced by `anyDuplicated(x$col) == 0L`.
#'
#' @evalRd rd_tags("any_duplicated_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
any_duplicated_linter <- function() {
  any_duplicated_xpath <- "//expr[
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'any']]
    and expr[expr[1][SYMBOL_FUNCTION_CALL[text() = 'duplicated']]]
    and (
      not(OP-COMMA)
      or OP-COMMA[
        not(preceding-sibling::OP-COMMA)
        and following-sibling::SYMBOL_SUB[1][text() = 'na.rm']
      ]
    )
  ]"

  # outline:
  #   EQ/NE/GT/LT: ensure we're in a comparison clause
  #   check for length(unique()) matching:
  #     - length(unique( _ )) == length( _ )
  #     - length(unique( << _$col or _[["col"]] >> )) == nrow( _ )
  # NB: parent::expr/.../following-sibling::expr is the path from
  #  the expr of the unique() call to the call that needs to match.
  #  the final parent::expr/expr gets us to the expr on the other side of EQ;
  #  this lets us match on either side of EQ, where following-sibling
  #  assumes we are before EQ, preceding-sibling assumes we are after EQ.
  length_unique_xpath <- "
  //expr[EQ or NE or GT or LT]
  /expr[
    expr[1][SYMBOL_FUNCTION_CALL[text() = 'length']]
    and expr[expr[1][
      SYMBOL_FUNCTION_CALL[text() = 'unique']
      and (
        following-sibling::expr =
          parent::expr
          /parent::expr
          /parent::expr
          /expr
          /expr[1][SYMBOL_FUNCTION_CALL[text()= 'length']]
          /following-sibling::expr
        or
        following-sibling::expr[OP-DOLLAR or LBB]/expr[1] =
          parent::expr
          /parent::expr
          /parent::expr
          /expr
          /expr[1][SYMBOL_FUNCTION_CALL[text()= 'nrow']]
          /following-sibling::expr
      )
    ]]
  ]"

  uses_nrow_xpath <- "./parent::expr/expr/expr[1]/SYMBOL_FUNCTION_CALL[text() = 'nrow']"

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    any_duplicated_expr <- xml2::xml_find_all(xml, any_duplicated_xpath)
    any_duplicated_lints <- xml_nodes_to_lints(
      any_duplicated_expr,
      source_expression = source_expression,
      lint_message = "anyDuplicated(x, ...) > 0 is better than any(duplicated(x), ...).",
      type = "warning"
    )

    length_unique_expr <- xml2::xml_find_all(xml, length_unique_xpath)
    lint_message <- ifelse(
      is.na(xml2::xml_find_first(length_unique_expr, uses_nrow_xpath)),
      "anyDuplicated(x) == 0L is better than length(unique(x)) == length(x).",
      "anyDuplicated(DF$col) == 0L is better than length(unique(DF$col)) == nrow(DF)"
    )
    length_unique_lints <- xml_nodes_to_lints(
      length_unique_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )

    return(c(any_duplicated_lints, length_unique_lints))
  })
}
