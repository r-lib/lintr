#' @describeIn linters Check that all left parentheses have a space before them
#' unless they are in a function call.
#' @export
spaces_left_parentheses_linter <- function() {
  Linter(function(source_file) {

    if (is.null(source_file$xml_parsed_content)) return(list())

    xml <- source_file$xml_parsed_content

    function_cond <-
      "@start = preceding-sibling::*[not(self::FUNCTION or self::expr[SYMBOL_FUNCTION_CALL or FUNCTION])]/@end + 1"
    for_cond <- "parent::forcond[@start = preceding-sibling::FOR/@end + 1]"
    mult_cond <- "parent::expr[@start = preceding-sibling::OP-STAR/@end + 1]"

    paren_cond <- sprintf("(%s)", paste(function_cond, for_cond, mult_cond, sep = ") or ("))
    xpath <- sprintf("//OP-LEFT-PAREN[%s]", paren_cond)

    bad_paren <- xml2::xml_find_all(xml, xpath)

    lapply(bad_paren, xml_nodes_to_lint, source_file,
           message = "Place a space before left parenthesis, except in a function call.",
           type = "style")
  })
}
