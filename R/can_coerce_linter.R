#' Require usage of `methods::canCoerce(x, cl)` over `try(as(x, cl))`
#'
#' [methods::canCoerce()] exists specifically for testing whether an object can
#'   be coerced to a given S4 class. Wrapping [methods::as()] in [base::try()]
#'   or [base::tryCatch()] to test coercibility is slower and less expressive.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'try(as(x, "numeric"))',
#'   linters = can_coerce_linter()
#' )
#'
#' lint(
#'   text = 'tryCatch(as(x, "numeric"), error = function(e) NULL)',
#'   linters = can_coerce_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'canCoerce(x, "numeric")',
#'   linters = can_coerce_linter()
#' )
#'
#' lint(
#'   text = 'try(as(x, "numeric", strict = FALSE))',
#'   linters = can_coerce_linter()
#' )
#'
#' @evalRd rd_tags("can_coerce_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
can_coerce_linter <- function() {
  not_kwarg_cond <- "not(preceding-sibling::*[not(self::COMMENT)][1][self::EQ_SUB])"
  pipe_op <- glue("PIPE or SPECIAL[{xp_text_in_table(magrittr_pipes)}]")

  is_as_call <- glue("
    (
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'as']]
      and count(expr) = 3
      and not(SYMBOL_SUB[text() = 'strict' or text() = 'ext'])
    ) or (
      ({pipe_op})
      and expr[2][
        expr[1][SYMBOL_FUNCTION_CALL[text() = 'as']]
        and count(expr) = 2
        and not(SYMBOL_SUB[text() = 'strict' or text() = 'ext'])
      ]
    )
  ")

  xpath <- glue("
  parent::expr[
    SYMBOL_SUB[text() = 'expr']/following-sibling::expr[1][{is_as_call}]
    or (
      not(SYMBOL_SUB[text() = 'expr'])
      and (
        expr[{not_kwarg_cond}][2][{is_as_call}]
        or parent::expr[({pipe_op}) and expr[1][{is_as_call}]]
      )
    )
  ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(c("try", "tryCatch"))
    bad_expr <- xml_find_all_(xml_calls, xpath)

    matched_function <- xp_call_name(bad_expr)
    lint_message <- sprintf("Use methods::canCoerce(x, cl) instead of %s(as(x, cl)).", matched_function)

    xml_nodes_to_lints(bad_expr, source_expression, lint_message = lint_message, type = "warning")
  })
}
