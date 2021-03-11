#' @describeIn linters check that there is a space between right
#' parenthesis and an opening curly brace.
#'
#' @export

paren_brace_linter <- function() {
  Linter(function(source_file) {
    if (is.null(source_file$xml_parsed_content)) {
      return(NULL)
    }

    xml <- source_file$xml_parsed_content

    xpath <- paste(
      "//OP-LEFT-BRACE[",
      "@line1 = parent::expr/preceding-sibling::OP-RIGHT-PAREN/@line1",
      "and",
      "@col1 = parent::expr/preceding-sibling::OP-RIGHT-PAREN/@col1 + 1",
      "]"
    )

    match_exprs <- xml2::xml_find_all(xml, xpath)

    lapply(
      match_exprs,
      function(expr) {
        x <- xml2::as_list(expr)
        line_num <- x@line1
        line <- source_file$lines[[as.character(line_num)]]
        Lint(
          filename = source_file$filename,
          line_number = line_num,
          column_number = x@col1,
          type = "style",
          message = "There should be a space between right parenthesis and an opening curly brace.",
          line = line,
          ranges = list(as.numeric(c(x@col1, x@col2)))
        )
      }
    )
  })
}
