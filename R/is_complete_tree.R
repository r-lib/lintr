#' Is this an expression- or a file-level source object?
#'
#' Helper for determining whether the current `source_expression` contains
#'   the full file tree, or is just a single expression.
#'
#' @param source_expresssion A parsed expression object, i.e., an element
#'   of the object returned by [get_source_expressions()].
#' @param level Which level of expression is being tested? `"expression"`
#'   means an individual expression, while `"file"` means the full
#'   source tree for this file.
#' @param require_xml If `TRUE`, the XML equivalent of the parse tree
#'   is required. Use this whenever writing a linter based on logic
#'   expressed via Xpath.
#'
#' @export
is_lint_level <- function(source_expression, level = c("expression", "file"), require_xml = FALSE) {
  stopifnot(!missing(level))
  level <- match.arg(level)
  required_key <- paste0(if (level == "complete") "full_", if (require_xml), "xml_", "parsed_content")
  !is.null(source_expression[[required_key]])
}
