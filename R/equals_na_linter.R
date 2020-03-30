#' @describeIn linters that checks for x == NA
#'
#' @export
equals_na_linter <- function(source_file) {
  if (!is.null(source_file[["file_lines"]])) {
    # abort if source_file is entire file, not a top level expression.
    return(NULL)
  }

  xml <- source_file$xml_parsed_content

  # match on the '=='
  xpath <- paste0(
    "//EQ[",
    "text() = '==' ",
    "and following-sibling::*/NUM_CONST[text() = 'NA']",
    "]"
  )

  eq_na_exprs <- xml2::xml_find_all(xml, xpath)

  lapply(
    eq_na_exprs,
    function(expr) {
      x <- as_list(expr)
      line_num <- x@line1
      line <- source_file[["lines"]][[as.character(line_num)]]
      Lint(
        filename = source_file$filename,
        line_number = line_num,
        column_number = x@col1,
        type = "warning",
        message = "Use is.na rather than == NA.",
        line = line,
        ranges = list(as.numeric(c(x@col1, x@col2))),
        "equals_na_linter"
      )
    }
  )
}
