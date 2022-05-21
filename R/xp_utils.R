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

#' Convert an XML node or nodeset into a Lint
#'
#' Convenience function for converting nodes matched by XPath-based
#'   linter logic into a [Lint()] object to return.
#'
#' @inheritParams Lint
#' @param xml An `xml_node` object (to generate one `Lint`) or an
#'   `xml_nodeset` object (to generate several `Lint`s), e.g. as returned by
#'   [xml2::xml_find_all()] or [xml2::xml_find_first()].
#' @param source_expression A source expression object, e.g. as
#'   returned typically by [lint()], or more generally
#'   by [get_source_expressions()].
#' @param lint_message The message to be included as the `message`
#'   to the `Lint` object. If `lint_message` is a `function`,
#'   this function is first applied to `xml` (so it should be a
#'   function taking an `xml_node` as input and must produce a
#'   length-1 character as output).
#' @param match_after_end Logical, default `FALSE`. If `TRUE`,
#'   The output `column_number` and `ranges[2L]` are set to _after_ the matched
#'   symbol in `xml`. This can be convenient for setting the source marker
#'   to land the editor's cursor exactly to the spot where corrected code can be
#'   entered. For example, in [T_and_F_symbol_linter()], in code using `T`, the
#'   cursor lands _after_ `T` so that `RUE` and be entered without further adjustment.
#' @return For `xml_node`s, a `lint`. For `xml_nodeset`s, `lints` (a list of `lint`s).
#' @export
xml_nodes_to_lints <- function(xml, source_expression, lint_message,
                               type = c("style", "warning", "error"),
                               match_after_end = FALSE) {
  if (length(xml) == 0L) {
    return(list())
  }
  if (inherits(xml, "xml_nodeset")) {
    lints <- lapply(xml, xml_nodes_to_lints, source_expression, lint_message, type, match_after_end)
    class(lints) <- "lints"
    return(lints)
  } else if (!inherits(xml, "xml_node")) {
    stop("Expected an xml_nodeset or xml_node, got an object of class(es): ", toString(class(xml)))
  }
  type <- match.arg(type, c("style", "warning", "error"))
  line1 <- xml2::xml_attr(xml, "line1")
  col1 <- as.integer(xml2::xml_attr(xml, "col1"))

  lines <- source_expression[["lines"]]
  if (is.null(lines)) lines <- source_expression[["file_lines"]]

  if (xml2::xml_attr(xml, "line2") == line1) {
    col2 <- as.integer(xml2::xml_attr(xml, "col2"))
  } else {
    col2 <- unname(nchar(lines[line1]))
  }

  if (match_after_end) {
    column_number <- col2 <- col2 + 1L
  } else {
    column_number <- col1
  }

  if (is.function(lint_message)) lint_message <- lint_message(xml)
  Lint(
    filename = source_expression$filename,
    line_number = as.integer(line1),
    column_number = column_number,
    type = type,
    message = lint_message,
    line = lines[line1],
    ranges = list(c(col1, col2))
  )
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
