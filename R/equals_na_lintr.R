#' @describeIn linters that checks for x == NA
#' @export
equals_na_linter <- function(source_file) {

  if (!length(source_file$parsed_content)) return(list())

  xml <- source_file$xml_parsed_content

  comparators <- c("EQ", "NE")
  comparator_table <- paste0("self::", comparators, collapse = " or ")
  NA_values <- c("NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
  NA_table <- paste("text() =", sQuote(NA_values, "'"), collapse = " or ")

  xpath_fmt <- "//expr[expr[NUM_CONST[%s]]]/*[%s]"
  xpath <- sprintf(xpath_fmt, NA_table, comparator_table)

  bad_expr <- xml2::xml_find_all(xml, xpath)

  lapply(bad_expr, xml_nodes_to_lint, source_file,
         message = "Use is.na for comparisons to NA (not == or !=)",
         linter = "equals_na_linter", type = "warning")
}
