# utils for working with xpaths

# like `text() %in% table`, translated to XPath 1.0
xp_text_in_table <- function(table) {
  if (length(table) == 0L) {
    return("true")
  }
  # xpath doesn't seem to have a standard way of escaping quotes, so attempt
  #   to use "" whenever the string has ' (not a perfect solution). info on
  #   escaping from https://stackoverflow.com/questions/14822153
  single_quoted <- grepl("'", table, fixed = TRUE)
  table[single_quoted] <- sQuote(table[single_quoted], '"')
  table[!single_quoted] <- sQuote(table[!single_quoted], "'")
  paste0("text() = ", table, collapse = " or ")
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

#' Get the name of the function matched by an XPath
#'
#' Often, it is more helpful to tailor the `message` of a lint to record
#'   which function was matched by the lint logic. This function encapsulates
#'   the logic to pull out the matched call in common situations.
#'
#' @param expr An `xml_node` or `xml_nodeset`, e.g. from [xml2::xml_find_all()].
#' @param depth Integer, default `1L`. How deep in the AST represented by `expr`
#'   should we look to find the call? By default, we assume `expr` is matched
#'   to an `<expr>` node under which the corresponding `<SYMBOL_FUNCTION_CALL>`
#'   node is found directly. `depth = 0L` means `expr` is matched directly
#'   to the `SYMBOL_FUNCTION_CALL`; `depth > 1L` means `depth` total `<expr>`
#'   nodes must be traversed before finding the call.
#' @param condition An additional (XPath condition on the `SYMBOL_FUNCTION_CALL`
#'   required for a match. The default (`NULL`) is no condition. See examples.
#'
#' @examples
#' xml_from_code <- function(str) {
#'   xml2::read_xml(xmlparsedata::xml_parse_data(parse(text = str, keep.source = TRUE)))
#' }
#' xml <- xml_from_code("sum(1:10)")
#' xp_call_name(xml, depth = 2L)
#'
#' xp_call_name(xml2::xml_find_first(xml, "expr"))
#'
#' xml <- xml_from_code(c("sum(1:10)", "sd(1:10)"))
#' xp_call_name(xml, depth = 2L, condition = "text() = 'sum'")
#'
#' @export
xp_call_name <- function(expr, depth = 1L, condition = NULL) {
  stopifnot(
    is.numeric(depth), depth >= 0L,
    is.null(condition) || is.character(condition)
  )
  is_valid_expr <- is_node(expr) || is_nodeset(expr)
  if (!is_valid_expr) {
    cli_abort(c(
      i = "{.arg expr} must be an {.cls xml_nodeset} or an {.cls xml_node}.",
      x = "Instead, it is {.obj_type_friendly {expr}}."
    ))
  }

  if (is.null(condition)) {
    node <- "SYMBOL_FUNCTION_CALL"
  } else {
    node <- sprintf("SYMBOL_FUNCTION_CALL[%s]", condition)
  }

  xpath <- paste0("string(", strrep("expr/", depth), node, ")")

  xml_find_chr(expr, xpath)
}

xp_find_location <- function(xml, xpath) {
  if (identical(xpath, "number(./@col1)")) {
    as.integer(xml_attr(xml, "col1"))
  } else if (identical(xpath, "number(./@col2)")) {
    as.integer(xml_attr(xml, "col2"))
  } else {
    as.integer(xml_find_num(xml, xpath))
  }
}

#' Strip XPath 2.0-style comments from an XPath
#'
#' `{xml2}` uses XPath 1.0, which has no support for comments. But comments are
#'   useful in a codebase with as many XPaths as we maintain, so we fudge our
#'   way to XPath 2.0-ish support by writing this simple function to remove comments.
#'
#' @noRd
xpath_comment_re <- rex(
  "(:",
  zero_or_more(not(":)")),
  ":)"
)
xp_strip_comments <- function(xpath) rex::re_substitutes(xpath, xpath_comment_re, "", global = TRUE)

#' Combine two or more nodesets to a single nodeset
#'
#' Useful for calling `{xml2}` functions on a combined set of nodes obtained using different XPath searches.
#'
#' @noRd
# TODO(r-lib/xml2#433): remove this and just use c()
combine_nodesets <- function(...) {
  res <- c(...)
  class(res) <- "xml_nodeset"
  res
}
