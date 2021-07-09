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
      "|",
      "preceding-sibling::IF/@line1",
      "|",
      "preceding-sibling::WHILE/@line1",
      "and",
      "@col1 = preceding-sibling::OP-RIGHT-PAREN/@col1 + 1",
      "]",
      "|",
      "//expr[",
      "@line1 = preceding-sibling::forcond/@line1",
      "and",
      "@col1 = preceding-sibling::forcond/OP-RIGHT-PAREN/@col1 + 1",
      "]"
    )
    matched_expressions <- xml2::xml_find_all(source_file$xml_parsed_content, xpath)

    lapply(
      matched_expressions,
      xml_nodes_to_lint,
      source_file = source_file,
      message = "There should be a space between right parenthesis and a body expression."
    )
  })
}
