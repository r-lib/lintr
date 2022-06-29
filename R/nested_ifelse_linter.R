#' Block usage of nested ifelse() calls
#'
#' Calling `ifelse` in nested calls is problematic for two main reasons:
#'   1. It can be hard to read -- mapping the code to the expected output
#'      for such code can be a messy task/require a lot of mental bandwidth,
#'      especially for code that nests more than once
#'   2. It is inefficient -- `ifelse` can evaluate _all_ of its arguments at
#'      both yes and no (see https://stackoverflow.com/q/16275149); this issue
#'      is exacerbated for nested calls
#'
#' Users can instead rely on a more readable alternative modeled after SQL
#'   CASE WHEN statements, such as `data.table::fcase` or `dplyr::case_when`,
#'   or use a look-up-and-merge approach (build a mapping table between values
#'   and outputs and merge this to the input).
#'
#' @evalRd rd_tags("nested_ifelse_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
nested_ifelse_linter <- function() {
  xpath <- glue::glue("
  //expr[expr[1][SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]]
  /expr[expr[1][SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    matched_call <- xp_call_name(bad_expr)
    lint_message <- paste(
      sprintf("Don't use nested %s() calls;", matched_call),
      "instead, try (1) data.table::fcase; (2) dplyr::case_when; or (3) using a lookup table."
    )
    xml_nodes_to_lints(bad_expr, source_expression, lint_message, type = "warning")
  })
}

# functions equivalent to base::ifelse() for linting purposes
# NB: this is re-used elsewhere, e.g. in ifelse_censor_linter
ifelse_funs <- c("ifelse", "if_else", "fifelse")
