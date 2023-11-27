#' Build the `xml_find_function_calls()` helper for a source expression
#'
#' @param xml The XML parse tree as an XML object (`xml_parsed_content` or `full_xml_parsed_content`)
#'
#' @return A fast function to query
#' `xml_find_all(xml, glue::glue("//SYMBOL_FUNCTION_CALL[{ xp_text_in_table(function_names) }]"))`.
#'
#' @noRd
build_xml_find_function_calls <- function(xml) {
  function_call_cache <- xml_find_all(xml, "//SYMBOL_FUNCTION_CALL")
  names(function_call_cache) <- get_r_string(function_call_cache)

  function(function_names) {
    unname(function_call_cache[names(function_call_cache) %in% function_names])
  }
}
