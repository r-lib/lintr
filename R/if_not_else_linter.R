#' Block statements like if (!A) x else y
#'
#' `if (!A) x else y` is the same as `if (A) y else x`, but the latter is
#'   easier to reason about in the `else` case. The former requires
#'   double negation that can be avoided by switching the statement order.
#'
#' This only applies in the simple `if/else` case. Statements like
#'   `if (!A) x else if (B) y else z` don't always have a simpler or
#'   more readable form.
#'
#' It also applies to [ifelse()] and the package equivalents
#'   `dplyr::if_else()` and `data.table::fifelse()`.
#'
#' @param exceptions Character vector of calls to exclude from linting.
#'   By default, [is.null()], [is.na()], and [missing()] are excluded
#'   given the common idiom `!is.na(x)` as "x is present".
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "if (!A) x else y",
#'   linters = if_not_else_linter()
#' )
#'
#' lint(
#'   text = "ifelse(!is_treatment, x, y)",
#'   linters = if_not_else_linter()
#' )
#'
#' lint(
#'   text = "if (!is.null(x)) x else 2",
#'   linters = if_not_else_linter(exceptions = character())
#' )
#'
#' # okay
#' lint(
#'   text = "if (A) x else y",
#'   linters = if_not_else_linter()
#' )
#'
#' lint(
#'   text = "ifelse(is_treatment, y, x)",
#'   linters = if_not_else_linter()
#' )
#'
#' lint(
#'   text = "if (!is.null(x)) x else 2",
#'   linters = if_not_else_linter()
#' )
#'
#' @evalRd rd_tags("if_not_else_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
if_not_else_linter <- function(exceptions = c("is.null", "is.na", "missing")) {
  if_xpath <- glue("
  //IF[following-sibling::ELSE[not(following-sibling::expr[IF])]]
    /following-sibling::expr[1][
      OP-EXCLAMATION
      and not(expr[expr[SYMBOL_FUNCTION_CALL[{ xp_text_in_table(exceptions) }]]])
    ]
  ")

  ifelse_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]
    /parent::expr
    /parent::expr[expr[
      position() = 2
      and OP-EXCLAMATION
      and not(expr[
        OP-EXCLAMATION
        or expr/SYMBOL_FUNCTION_CALL[{ xp_text_in_table(exceptions) }]
      ])
    ]]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    if_expr <- xml_find_all(xml, if_xpath)
    if_lints <- xml_nodes_to_lints(
      if_expr,
      source_expression = source_expression,
      lint_message = paste(
        "In a simple if/else statement,",
        "prefer `if (A) x else y` to the less-readable `if (!A) y else x`."
      ),
      type = "warning"
    )

    ifelse_expr <- xml_find_all(xml, ifelse_xpath)
    ifelse_call <- xp_call_name(ifelse_expr)
    ifelse_lints <- xml_nodes_to_lints(
      ifelse_expr,
      source_expression = source_expression,
      lint_message = sprintf(
        "Prefer `%1$s(A, x, y)` to the less-readable `%1$s(!A, y, x)`.",
        ifelse_call
      ),
      type = "warning"
    )

    c(if_lints, ifelse_lints)
  })
}