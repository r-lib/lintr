#' Is this an expression- or a file-level source object?
#'
#' Helper for determining whether the current `source_expression` contains
#'   the full file tree, or is just a single expression.
#'
#' @export
is_lint_level <- function(source_file, level, require_xml = FALSE) {
  level <- match.arg(level, level = c("expression", "complete"))
  required_key <- paste0(if (level == "complete") "full_", if (require_xml), "xml_", "parsed_content")
  !is.null(source_file[[required_key]])
}
