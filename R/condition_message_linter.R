#' Block usage of paste() and paste0() with messaging functions using ...
#'
#' `stop(paste0(...))` is strictly redundant -- `stop(...)` is equivalent.
#'   `stop(...)` is also preferable to `stop(paste(...))`. The same applies to
#'   all default condition functions, i.e., [stop()], [warning()], [message()],
#'   and [packageStartupMessage()].
#'
#' @evalRd rd_tags("condition_message_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
condition_message_linter <- function() {
  translators <- c("packageStartupMessage", "message", "warning", "stop")
  xpath <- glue::glue("//expr[
    expr[1][SYMBOL_FUNCTION_CALL[ {xp_text_in_table(translators)} ]]
    and expr[
      expr[1][SYMBOL_FUNCTION_CALL[text() = 'paste' or text() = 'paste0']]
      and not(SYMBOL_SUB[text() = 'collapse'])
    ]
  ]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)
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
