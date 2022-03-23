#' Require usage of anyDuplicated() > 0 over any(duplicated(.))
#'
#' [anyDuplicated()] exists as a replacement for `any(duplicated(.))` which is
#'   more efficient for simple objects, and in the worst case is the same
#'   efficiency. Therefore it should be used in all situations instead of the
#'   latter.
#'
#' Also match usage like `length(unique(x$col)) == nrow(x)`, which can
#'   be replaced by `anyDuplicated(x$col) == 0L`.
#'
#' @evalRd rd_tags("any_duplicated_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
any_duplicated_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    any_duplicated_xpath <- "//expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'any']]
      and expr[expr[SYMBOL_FUNCTION_CALL[text() = 'duplicated']]]
      and (
        not(OP-COMMA)
        or OP-COMMA[
          not(preceding-sibling::OP-COMMA)
          and following-sibling::SYMBOL_SUB[1][text() = 'na.rm']
        ]
      )
    ]"

    any_duplicated_expr <- xml2::xml_find_all(xml, any_duplicated_xpath)

    # path from the expr of the unique() call to the call that needs to match.
    #  the final parent::expr/expr gets us to the expr on the other side of EQ;
    #  this lets us match on either side of EQ, where following-sibling
    #  assumes we are before EQ, preceding-sibling assumes we are after EQ.
    path_to_neighbor_call_expr_fmt <- file.path(
      "parent::expr",
      "parent::expr",
      "parent::expr",
      "expr",
      "expr[SYMBOL_FUNCTION_CALL[text()= '%s']]",
      "following-sibling::expr"
    )
    unique_expr_xpath <- xp_and(
      "SYMBOL_FUNCTION_CALL[text() = 'unique']",
      # ensure the expr matches to avoid spurious match like
      #   length(unique(x)) == length(y)
      xp_or(
        # length(unique(x)) == length(x).
        sprintf(
          "following-sibling::expr = %s",
          sprintf(path_to_neighbor_call_expr_fmt, "length")
        ),
        # length(unique( << DF$col or DF[["col"]] >> )) == nrow(DF)
        sprintf(
          "following-sibling::expr[OP-DOLLAR or LBB]/expr[1] = %s",
          sprintf(path_to_neighbor_call_expr_fmt, "nrow")
        )
      )
    )
    length_unique_call_xpath <- xp_and(
      "expr[SYMBOL_FUNCTION_CALL[text() = 'length']]",
      sprintf("expr[expr[%s]]", unique_expr_xpath)
    )
    # EQ ensures we're in an ==, !=, <, or > clause
    length_unique_xpath <-
      sprintf("//expr[EQ or NE or GT or LT]/expr[%s]", length_unique_call_xpath)
    length_unique_expr <- xml2::xml_find_all(xml, length_unique_xpath)

    return(lapply(
      c(any_duplicated_expr, length_unique_expr),
      xml_nodes_to_lint,
      source_file = source_file,
      message = paste(
        "anyDuplicated(x, ...) > 0 is better than any(duplicated(x), ...).",
        "anyDuplicated(x) == 0L is better than length(unique(x)) == length(x)",
        "and length(unique(DF$col)) == nrow(DF)."
      ),
      type = "warning"
    ))
  })
}
