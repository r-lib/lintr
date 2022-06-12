# utils for working with xpaths

# like `text() %in% table`, translated to XPath 1.0
xp_text_in_table <- function(table) {
  # xpath doesn't seem to have a standard way of escaping quotes, so attempt
  #   to use "" whenever the string has ' (not a perfect solution). info on
  #   escaping from https://stackoverflow.com/questions/14822153
  single_quoted <- grepl("'", table, fixed = TRUE)
  table[single_quoted] <- quote_wrap(table[single_quoted], '"')
  table[!single_quoted] <- quote_wrap(table[!single_quoted], "'")
  return(paste0("text() = ", table, collapse = " or "))
}

paren_wrap <- function(..., sep) {
  sep <- paste(")", sep, "(")
  dots <- list(...)
  if (length(dots) == 1L && length(dots[[1L]]) > 1L) {
    inner <- paste(dots[[1L]], collapse = sep)
  } else {
    inner <- paste(..., sep = sep)
  }
  paste0("(", inner, ")")
}

#' Safer wrapper for paste(..., sep = " and ")
#'
#' The intent is to use this for readability when combining XPath conditions so
#'   the `...` elements should be in that language, but this is not enforced.
#'
#' Inputs are also wrapped in `()` so as not to risk mixing operator precedence.
#'
#' @param ... Series of conditions
#' @noRd
xp_and <- function(...) paren_wrap(..., sep = "and")

#' Safer wrapper for paste(..., sep = " or ")
#'
#' The intent is to use this for readability when combining XPath conditions so
#'   the `...` elements should be in that language, but this is not enforced.
#'
#' Inputs are also wrapped in `()` so as not to risk mixing operator precedence.
#'
#' @param ... Series of conditions
#' @noRd
xp_or <- function(...) paren_wrap(..., sep = "or")

xp_call_name <- function(expr, depth = 1L, condition = NULL) {
  if (is.null(condition)) {
    node <- "SYMBOL_FUNCTION_CALL"
  } else {
    node <- sprintf("SYMBOL_FUNCTION_CALL[%s]", condition)
  }

  xpath <- paste0("string(", strrep("expr/", depth), node, ")")

  xml2::xml_find_chr(expr, xpath)
}

xp_find_location <- function(xml, xpath) {
  if (identical(xpath, "number(./@col1)")) {
    as.integer(xml2::xml_attr(xml, "col1"))
  } else if (identical(xpath, "number(./@col2)")) {
    as.integer(xml2::xml_attr(xml, "col2"))
  } else {
    as.integer(xml2::xml_find_num(xml, xpath))
  }
}
