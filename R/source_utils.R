#' Build the `xml_find_function_calls()` helper for a source expression
#'
#' @param xml The XML parse tree as an XML object (`xml_parsed_content` or `full_xml_parsed_content`)
#'
#' @return A fast function to query
#' `xml_find_all(xml, glue::glue("//SYMBOL_FUNCTION_CALL[text() = '{function_names[1]}' or ...]"))`,
#' or, using the internal function `xp_text_in_table()`,
#' `xml_find_all(xml, glue::glue("//SYMBOL_FUNCTION_CALL[{ xp_text_in_table(function_names) }]"))`.
#'
#' @noRd
build_xml_find_function_calls <- function(xml) {
  function_call_cache <- xml_find_all(xml, "//SYMBOL_FUNCTION_CALL")
  names(function_call_cache) <- get_r_string(function_call_cache)

  function(function_names, keep_names = FALSE, land_on = c("call_symbol", "call_symbol_expr", "call_expr")) {
    land_on <- match.arg(land_on)
    if (is.null(function_names)) {
      res <- function_call_cache
    } else {
      res <- function_call_cache[names(function_call_cache) %in% function_names]
    }
    if (!keep_names) res <- unname(res)
    switch(land_on,
      call_symbol = res,
      call_symbol_expr = xml_find_first(res, "parent::expr"),
      call_expr = xml_find_first(res, "parent::expr/parent::expr")
    )
  }
}
