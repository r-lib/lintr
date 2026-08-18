#' Build the `xml_find_function_calls()` helper for a source expression
#'
#' @param xml The XML parse tree as an XML object (`xml_parsed_content` or `full_xml_parsed_content`)
#'
#' @return A fast function to query the common XPath expression
#'   `xml_find_all_(xml, glue::glue("//SYMBOL_FUNCTION_CALL[text() = '{function_names[1]}' or ...]/parent::expr"))`,
#'   or, using the internal function `xp_text_in_table()`,
#'   `xml_find_all_(xml, glue::glue("//SYMBOL_FUNCTION_CALL[{ xp_text_in_table(function_names) }]/parent::expr"))`,
#'   i.e., the `parent::expr` of the `SYMBOL_FUNCTION_CALL` node corresponding to given function names.
#'
#' @noRd
build_xml_find_function_calls <- function(xml) {
  force(xml)
  cache_env <- new.env(parent = emptyenv())
  cache_env$function_call_cache <- NULL
  cache_env$s4_slot_cache <- NULL

  function(function_names, keep_names = FALSE, include_s4_slots = FALSE) {
    if (is.null(cache_env$function_call_cache)) {
      cache_env$function_call_cache <- xml_find_all_(xml, "//SYMBOL_FUNCTION_CALL/parent::*")
      names(cache_env$function_call_cache) <- get_r_string(cache_env$function_call_cache, "SYMBOL_FUNCTION_CALL")
    }
    if (include_s4_slots && is.null(cache_env$s4_slot_cache)) {
      cache_env$s4_slot_cache <- xml_find_all_(xml, "//SLOT/parent::expr[following-sibling::OP-LEFT-PAREN]")
      names(cache_env$s4_slot_cache) <- get_r_string(cache_env$s4_slot_cache, "SLOT")
    }

    if (is.null(function_names)) {
      if (include_s4_slots) {
        res <- combine_nodesets(cache_env$function_call_cache, cache_env$s4_slot_cache)
      } else {
        res <- cache_env$function_call_cache
      }
    } else {
      include_function_idx <- names(cache_env$function_call_cache) %in% function_names
      if (include_s4_slots) {
        res <- combine_nodesets(
          cache_env$function_call_cache[include_function_idx],
          cache_env$s4_slot_cache[names(cache_env$s4_slot_cache) %in% function_names]
        )
      } else {
        res <- cache_env$function_call_cache[include_function_idx]
      }
    }
    if (keep_names) res else unname(res)
  }
}
