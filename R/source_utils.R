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
  function_call_cache <- xml_parent(xml_find_all(xml, "//SYMBOL_FUNCTION_CALL"))
  names(function_call_cache) <- get_r_string(function_call_cache, "SYMBOL_FUNCTION_CALL")

  s4_slot_cache <- xml_find_all(xml, "//SLOT/parent::expr[following-sibling::OP-LEFT-PAREN]")
  names(s4_slot_cache) <- get_r_string(s4_slot_cache, "SLOT")

  function(function_names, keep_names = FALSE, include_s4_slots = FALSE) {
    if (is.null(function_names)) {
      if (include_s4_slots) {
        res <- combine_nodesets(function_call_cache, s4_slot_cache)
      } else {
        res <- function_call_cache
      }
    } else {
      include_function_idx <- names(function_call_cache) %in% function_names
      if (include_s4_slots) {
        res <- combine_nodesets(
          function_call_cache[include_function_idx],
          s4_slot_cache[names(s4_slot_cache) %in% function_names]
        )
      } else {
        res <- function_call_cache[include_function_idx]
      }
    }
    if (keep_names) res else unname(res)
  }
}
