#' Block usage of paste() and paste0() with messaging functions using ...
#'
#' `stop(paste0(...))` is strictly redundant -- `stop(...)` is equivalent.
#'   `stop(...)` is also preferable to `stop(paste(...))`.
#'
#' @evalRd rd_tags("stop_paste_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
stop_paste_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    translators <- c("packageStartupMessage", "message", "warning", "stop")
    # TODO: refactor to work for raw-string equivalents
    xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(translators)} ]]
      and expr[
        expr[SYMBOL_FUNCTION_CALL[text() = 'paste' or text() = 'paste0']]
        and not(SYMBOL_SUB[text() = 'collapse'])
        and (
          not(SYMBOL_SUB[text() = 'sep'])
          or SYMBOL_SUB[
            text() = 'sep'
            and following-sibling::expr[1]/STR_CONST[text() = '\"\"' or text() = '\" \"']
          ]
        )
      ]
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = function(expr) {
        outer_call <- xml2::xml_text(xml2::xml_find_first(expr, "expr/SYMBOL_FUNCTION_CALL"))
        inner_call <- xml2::xml_text(xml2::xml_find_first(expr, "expr/expr/SYMBOL_FUNCTION_CALL"))

        message <- sprintf("Don't use %s to build %s strings.", inner_call, outer_call)
        paste(
          message,
          "Instead use the fact that these functions build strings from their input",
          '(using "" as a separator). For translateable strings, prefer using gettextf().'
        )
       },
      type = "warning"
    ))
  })
}
