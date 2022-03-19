# utils for working with xpaths

# like `text() %in% table`, translated to XPath 1.0
xp_text_in_table <- function(table) paste("text() = ", quote_wrap(table, "'"), collapse = " or ")

# convert an XML match into a Lint
xml_nodes_to_lint <- function(xml, source_file, message,
                              type = c("style", "warning", "error"),
                              offset = 0L,
                              global = FALSE) {
  type <- match.arg(type, c("style", "warning", "error"))
  line1 <- xml2::xml_attr(xml, "line1")[1]
  col1 <- as.integer(xml2::xml_attr(xml, "col1")) + offset

  line_elt <- if (global) "file_lines" else "lines"

  if (xml2::xml_attr(xml, "line2") == line1) {
    col2 <- as.integer(xml2::xml_attr(xml, "col2")) + offset
  } else {
    col2 <- unname(nchar(source_file[[line_elt]][line1]))
  }
  return(Lint(
    filename = source_file$filename,
    line_number = as.integer(line1),
    column_number = as.integer(col1),
    type = type,
    message = message,
    line = source_file[[line_elt]][line1],
    ranges = list(c(col1 - offset, col2))
  ))
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
