#' Block usage of `paste()` and `paste0()` with messaging functions using `...`
#'
#' @description
#' This linter discourages combining condition functions like [stop()] with string concatenation
#'   functions [paste()] and [paste0()]. This is because
#'
#'  - `stop(paste0(...))` is redundant as it is exactly equivalent to `stop(...)`
#'  - `stop(paste(...))` is similarly equivalent to `stop(...)` with separators (see examples)
#'
#' The same applies to the other default condition functions as well, i.e., [warning()], [message()],
#'   and [packageStartupMessage()].
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'stop(paste("a string", "another"))',
#'   linters = condition_message_linter()
#' )
#'
#' lint(
#'   text = 'warning(paste0("a string", " another"))',
#'   linters = condition_message_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'stop("a string", " another")',
#'   linters = condition_message_linter()
#' )
#'
#' lint(
#'   text = 'warning("a string", " another")',
#'   linters = condition_message_linter()
#' )
#'
#' lint(
#'   text = 'warning(paste("a string", "another", sep = "-"))',
#'   linters = condition_message_linter()
#' )
#'
#' @evalRd rd_tags("condition_message_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
condition_message_linter <- function() {
  translators <- c("packageStartupMessage", "message", "warning", "stop")
  xpath <- glue("
  self::*[SYMBOL_FUNCTION_CALL[
    not(preceding-sibling::OP-DOLLAR or preceding-sibling::OP-AT)
  ]]
    /following-sibling::expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'paste' or text() = 'paste0']]
      and not(SYMBOL_SUB[text() = 'collapse'])
    ]
    /parent::expr
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(translators)
    bad_expr <- xml_find_all(xml_calls, xpath)
    sep_value <- get_r_string(bad_expr, xpath = "./expr/SYMBOL_SUB[text() = 'sep']/following-sibling::expr/STR_CONST")

    bad_expr <- bad_expr[is.na(sep_value) | sep_value %in% c("", " ")]
    outer_call <- xp_call_name(bad_expr)
    inner_call <- xp_call_name(bad_expr, depth = 2L)
    lint_message <- paste(
      "Don't use", inner_call, "to build", outer_call, "strings.",
      "Instead use the fact that these functions build condition message strings from their input",
      '(using "" as a separator). For translatable strings, prefer using gettextf().'
    )
    xml_nodes_to_lints(bad_expr, source_expression = source_expression, lint_message = lint_message, type = "warning")
  })
}
