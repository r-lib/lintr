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
  Linter(function(source_expression) {
    if (length(source_expression$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    xpath <- glue::glue("
    //expr[expr[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]]
    /expr[expr[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]]
    ")

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lint(
      bad_expr,
      source_expression = source_expression,
      lint_message = function(expr) {
        matched_call <- xml2::xml_text(xml2::xml_find_first(expr, "expr/SYMBOL_FUNCTION_CALL"))
        lint_message <- sprintf("Don't use nested %s() calls;", matched_call)
        paste(lint_message, "instead, try (1) data.table::fcase; (2) dplyr::case_when; or (3) using a lookup table.")
      },
      type = "warning"
    )
  })
}

# functions equivalent to base::ifelse() for linting purposes
# NB: this is re-used elsewhere, e.g. in ifelse_censor_linter
ifelse_funs <- c("ifelse", "if_else", "fifelse")
