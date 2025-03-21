#' Build the `xml_find_function_calls()` helper for a source expression
#'
#' @param xml The XML parse tree as an XML object (`xml_parsed_content` or `full_xml_parsed_content`)
#'
#' @return A fast function to query the common XPath expression
#'   `xml_find_all(xml, glue::glue("//SYMBOL_FUNCTION_CALL[text() = '{function_names[1]}' or ...]/parent::expr"))`,
#'   or, using the internal function `xp_text_in_table()`,
#'   `xml_find_all(xml, glue::glue("//SYMBOL_FUNCTION_CALL[{ xp_text_in_table(function_names) }]/parent::expr"))`,
#'   i.e., the `parent::expr` of the `SYMBOL_FUNCTION_CALL` node corresponding to given function names.
#'
#' @noRd
build_xml_find_function_calls <- function(xml) {
  function_call_cache <- xml_find_all(xml, "//SYMBOL_FUNCTION_CALL/parent::expr")
  names(function_call_cache) <- get_r_string(function_call_cache, "SYMBOL_FUNCTION_CALL")

  function(function_names, keep_names = FALSE) {
    if (is.null(function_names)) {
      res <- function_call_cache
    } else {
      res <- function_call_cache[names(function_call_cache) %in% function_names]
    }
    if (keep_names) res else unname(res)
  }
}
