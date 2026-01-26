#' Require usage of `anyDuplicated(x) > 0` over `any(duplicated(x))`
#'
#' [anyDuplicated()] exists as a replacement for `any(duplicated(.))`, which is
#'   more efficient for simple objects, and is at worst equally efficient.
#'   Therefore, it should be used in all situations instead of the latter.
#'
#' Also match usage like `length(unique(x$col)) == nrow(x)`, which can
#'   be replaced by `anyDuplicated(x$col) == 0L`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "any(duplicated(x), na.rm = TRUE)",
#'   linters = any_duplicated_linter()
#' )
#'
#' lint(
#'   text = "length(unique(x)) == length(x)",
#'   linters = any_duplicated_linter()
#' )
#'
#' lint(
#'   text = "DT[, uniqueN(col) == .N]",
#'   linters = any_duplicated_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "anyDuplicated(x)",
#'   linters = any_duplicated_linter()
#' )
#'
#' lint(
#'   text = "anyDuplicated(x) == 0L",
#'   linters = any_duplicated_linter()
#' )
#'
#' lint(
#'   text = "anyDuplicated(DT, by = 'col') == 0L",
#'   linters = any_duplicated_linter()
#' )
#'
#' @evalRd rd_tags("any_duplicated_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
any_duplicated_linter <- function() {
  any_duplicated_xpath <- "
  following-sibling::expr[1][expr[1]/SYMBOL_FUNCTION_CALL[text() = 'duplicated']]
    /parent::expr[
      count(expr) = 2
      or (count(expr) = 3 and SYMBOL_SUB[text() = 'na.rm'])
    ]
  "

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
  length_unique_xpath_parts <- glue("
  expr/expr[
    expr[1]/SYMBOL_FUNCTION_CALL[text() = 'length']
    and expr/expr[1][
      SYMBOL_FUNCTION_CALL[text() = 'unique']
      and (
        following-sibling::expr =
          parent::expr
            /parent::expr
            /parent::expr
            /expr
            /expr[1][SYMBOL_FUNCTION_CALL[text() = 'length']]
            /following-sibling::expr
        or following-sibling::expr[OP-DOLLAR or LBB]/expr[1] =
          parent::expr
            /parent::expr
            /parent::expr
            /expr
            /expr[1][SYMBOL_FUNCTION_CALL[text() = 'nrow']]
            /following-sibling::expr
        or parent::expr
          /parent::expr
          /parent::expr
          /expr[
            SYMBOL[text() = '.N']
            or (expr/SYMBOL_FUNCTION_CALL[text() = 'n'] and count(expr) = 1)
          ]
      )
    ]
  ]
  ")
  length_unique_xpath <- paste(length_unique_xpath_parts, collapse = " | ")

  distinct_xpath <- glue("
  expr/expr[
    expr[1][
      SYMBOL_FUNCTION_CALL[text() = 'uniqueN' or text() = 'n_distinct']
      and (
        following-sibling::expr =
          parent::expr
            /parent::expr
            /expr
            /expr[1][SYMBOL_FUNCTION_CALL[text() = 'length' or text() = 'nrow']]
            /following-sibling::expr
        or following-sibling::expr[OP-DOLLAR or LBB]/expr[1] =
          parent::expr
            /parent::expr
            /expr
            /expr[1][SYMBOL_FUNCTION_CALL[text() = 'nrow']]
            /following-sibling::expr
        or parent::expr
          /parent::expr
          /expr[
            SYMBOL[text() = '.N']
            or (expr/SYMBOL_FUNCTION_CALL[text() = 'n'] and count(expr) = 1)
          ]
      )
    ]
  ]
  ")

  uses_nrow_xpath <- "./parent::expr/expr/expr[1]/SYMBOL_FUNCTION_CALL[text() = 'nrow']"
  uses_dtn_xpath <- "./parent::expr/expr/SYMBOL[text() = '.N']"
  uses_dplyr_xpath <- "./parent::expr/expr/expr[1]/SYMBOL_FUNCTION_CALL[text() = 'n']"

  Linter(linter_level = "expression", function(source_expression) {
    # NB: need two parents given three parent::expr in XPath and stripped comments.
    xml <- source_expression$xml_parsed_content |>
      xml_find_all("(//EQ | //NE | //GT | //LT)/parent::*/parent::*") |>
      strip_comments_from_subtree()

    xml_calls <- source_expression$xml_find_function_calls("any")

    any_duplicated_expr <- xml_find_all(xml_calls, any_duplicated_xpath)
    any_duplicated_lints <- xml_nodes_to_lints(
      any_duplicated_expr,
      source_expression = source_expression,
      lint_message = "anyDuplicated(x, ...) > 0 is better than any(duplicated(x), ...).",
      type = "warning"
    )

    length_unique_expr <- xml_find_all(xml, length_unique_xpath)
    length_unique_lint_message <- character(length(length_unique_expr))
    length_unique_lint_message[] <- "anyDuplicated(x) == 0L is better than length(unique(x)) == length(x)."
    length_unique_lint_message[!is.na(xml_find_first(length_unique_expr, uses_nrow_xpath))] <-
      "anyDuplicated(DF$col) == 0L is better than length(unique(DF$col)) == nrow(DF)"
    length_unique_lint_message[!is.na(xml_find_first(length_unique_expr, uses_dtn_xpath))] <-
      "anyDuplicated(x) == 0L is better than length(unique(x)) == .N"
    length_unique_lint_message[!is.na(xml_find_first(length_unique_expr, uses_dplyr_xpath))] <-
      "anyDuplicated(x) == 0L is better than length(unique(x)) == n()."
    length_unique_lints <- xml_nodes_to_lints(
      length_unique_expr,
      source_expression = source_expression,
      lint_message = length_unique_lint_message,
      type = "warning"
    )

    distinct_expr <- xml_find_all(xml, distinct_xpath)
    distinct_lint_message_fmt <- character(length(distinct_expr))
    distinct_lint_message_fmt[] <- "anyDuplicated(x) == 0L is better than %s(x) == length(x)."
    distinct_lint_message_fmt[!is.na(xml_find_first(distinct_expr, uses_nrow_xpath))] <-
      "anyDuplicated(DF$col) == 0L is better than %s(DF$col) == nrow(DF)"
    distinct_lint_message_fmt[!is.na(xml_find_first(distinct_expr, uses_dtn_xpath))] <-
      "anyDuplicated(x) == 0L is better than %s(x) == .N"
    distinct_lint_message_fmt[!is.na(xml_find_first(distinct_expr, uses_dplyr_xpath))] <-
      "anyDuplicated(x) == 0L is better than %s(x) == n()."

    distinct_lint_message <- sprintf(distinct_lint_message_fmt, xp_call_name(distinct_expr))
    distinct_lints <- xml_nodes_to_lints(
      distinct_expr,
      source_expression = source_expression,
      lint_message = distinct_lint_message,
      type = "warning"
    )

    c(any_duplicated_lints, length_unique_lints, distinct_lints)
  })
}
