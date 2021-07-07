#' @describeIn linters check that there is a space between the
#' right parenthesis and the function body when no braces are used
#' to define a function.
#'
#' @export
paren_body_linter <- function() {
  Linter(function(source_file) {
    if (is.null(source_file$xml_parsed_content)) return(NULL)
    
    xpath <- paste(
      "//expr[",
      "@line1 = preceding-sibling::FUNCTION/@line1",
      "and",
      "@col1 = preceding-sibling::OP-RIGHT-PAREN/@col1 + 1",
      "]"
    )
    matched_expressions <- xml2::xml_find_all(source_file$xml_parsed_content, xpath)
    
    lapply(matched_expressions, get_lint_from_expression, source_file = source_file)
  })
}

get_lint_from_expression <- function(expression, source_file) {
  expression <- xml2::as_list(expression)
  Lint(
    filename = source_file$filename,
    message = "There should be a space between the right parenthesis and the function body.",
    column_number = expression@col1,
    line_number = expression@line1,
    line = source_file$lines[[as.character(expression@line1)]],
    ranges = list(as.integer(c(expression@col1, expression@col2)))
  )
}