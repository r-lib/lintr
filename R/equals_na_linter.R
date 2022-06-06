#' Equality check with NA linter
#'
#' Check for `x == NA` and `x != NA`
#'
#' @evalRd rd_tags("equals_na_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
equals_na_linter <- function() {
  comparators <- c("EQ", "NE")
  comparator_table <- paste0("self::", comparators, collapse = " or ")
  na_table <- xp_text_in_table(c("NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_"))

  xpath_fmt <- "//expr[expr[NUM_CONST[%s]]]/*[%s]"
  xpath <- sprintf(xpath_fmt, na_table, comparator_table)

  Linter(function(source_expression) {

    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression,
      lint_message = "Use is.na for comparisons to NA (not == or !=)",
      type = "warning"
    )
  })
}
