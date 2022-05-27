#' Undesirable function linter
#'
#' Report the use of undesirable functions, e.g. [base::return()], [base::options()], or
#' [base::sapply()] and suggest an alternative.
#'
#' @param fun Named character vector. `names(fun)` correspond to undesirable functions,
#'   while the values give a description of why the function is undesirable.
#'   If `NA`, no alternative is specified in the lint message. Defaults to
#'   [default_undesirable_functions]. To make small customizations to this list,
#'   use [modify_defaults()].
#' @param symbol_is_undesirable Whether to consider the use of an undesirable function name as a symbol undesirable
#'   or not.
#' @evalRd rd_tags("undesirable_function_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
undesirable_function_linter <- function(fun = default_undesirable_functions,
                                        symbol_is_undesirable = TRUE) {
  stopifnot(is.logical(symbol_is_undesirable))

  xp_condition <- xp_and(
    xp_text_in_table(names(fun)),
    paste0(
      "not(parent::expr/preceding-sibling::expr[SYMBOL_FUNCTION_CALL[",
      xp_text_in_table(c("library", "require")),
      "]])"
    ),
    "not(preceding-sibling::OP-DOLLAR)"
  )

  if (symbol_is_undesirable) {
    xpath <- glue::glue("//SYMBOL_FUNCTION_CALL[{xp_condition}] | //SYMBOL[{xp_condition}]")
  } else {
    xpath <- glue::glue("//SYMBOL_FUNCTION_CALL[{xp_condition}]")
  }


  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }
    matched_nodes <- xml2::xml_find_all(source_expression$xml_parsed_content, xpath)
    fun_names <- get_r_string(matched_nodes)

    msgs <- vapply(
      stats::setNames(nm = unique(fun_names)),
      function(fun_name) {
        msg <- sprintf('Function "%s" is undesirable.', fun_name)
        alternative <- fun[[fun_name]]
        if (!is.na(alternative)) {
          msg <- paste(msg, sprintf("As an alternative, %s.", alternative))
        }
        msg
      },
      character(1L)
    )

    xml_nodes_to_lints(
      matched_nodes,
      source_expression = source_expression,
      lint_message = unname(msgs[fun_names])
    )
  })
}
