unquote_names <- function(x) {
  gsub('^([`"\'])(.*)\\1(<-)?$', "\\2\\3", x, perl = TRUE)
}

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
  name_call_cache <- function(cache, node_type) {
    if (length(cache) == 0L) return(cache)
    call_names <- get_r_string(cache, node_type)
    is_setter <- xp_is_setter_call(cache)
    call_names[is_setter] <- paste0(call_names[is_setter], "<-")
    names(cache) <- call_names
    cache
  }

  function_call_xpath <- "
    //SYMBOL_FUNCTION_CALL/parent::*
    | //STR_CONST[not(preceding-sibling::OP-AT)]/parent::expr[following-sibling::OP-LEFT-PAREN]
  "
  function_call_cache <- name_call_cache(
    xml_find_all_(xml, function_call_xpath),
    "SYMBOL_FUNCTION_CALL | STR_CONST"
  )

  # not used much, so assign it lazily to delay the xml_find_all_ computation
  s4_slot_xpath <- "
    //SLOT/parent::expr[following-sibling::OP-LEFT-PAREN]
    | //OP-AT/following-sibling::STR_CONST/parent::expr[following-sibling::OP-LEFT-PAREN]
  "
  delayedAssign("s4_slot_cache", {
    name_call_cache(
      xml_find_all_(xml, s4_slot_xpath),
      "SLOT | STR_CONST"
    )
  })

  function(function_names, keep_names = FALSE, include_s4_slots = FALSE) {
    if (is.null(function_names)) {
      if (include_s4_slots) {
        res <- combine_nodesets(function_call_cache, s4_slot_cache)
      } else {
        res <- function_call_cache
      }
    } else {
      function_names <- unquote_names(function_names)
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
