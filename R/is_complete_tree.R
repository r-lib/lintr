#' Does this object contain the full file tree?
#'
#' Helper for determining whether the current expression contains the full file tree
#'
#' @export
is_tree_level <- function(source_file, level, require_xml = FALSE) {
  level <- match.arg(level, level = c("expression", "complete"))
  required_key <- paste0(if (level == "complete") "full_", if (require_xml), "xml_", "parsed_content")
  !is.null(source_file[[required_key]])
}
