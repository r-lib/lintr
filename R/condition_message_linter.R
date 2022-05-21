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
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    translators <- c("packageStartupMessage", "message", "warning", "stop")
    # TODO: refactor to work for raw-string equivalents
    xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(translators)} ]]
      and expr[
        expr[SYMBOL_FUNCTION_CALL[text() = 'paste' or text() = 'paste0']]
        and not(SYMBOL_SUB[text() = 'collapse'])
      ]
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)
    sep_value <- get_r_string(bad_expr, xpath = "./expr/SYMBOL_SUB[text() = 'sep']/following-sibling::expr/STR_CONST")

    xml_nodes_to_lints(
      bad_expr[is.na(sep_value) | sep_value %in% c("", " ")],
      source_expression = source_expression,
      lint_message = function(expr) {
        outer_call <- xp_call_name(expr)
        inner_call <- xp_call_name(expr, depth = 2L)

        message <- sprintf("Don't use %s to build %s strings.", inner_call, outer_call)
        paste(
          message,
          "Instead use the fact that these functions build condition message strings from their input",
          '(using "" as a separator). For translateable strings, prefer using gettextf().'
        )
       },
      type = "warning"
    )
  })
}
