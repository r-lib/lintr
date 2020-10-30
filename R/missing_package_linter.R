#' @describeIn linter that checks for missing packages
#' @export
missing_package_linter <- function(source_file) {

  if (!length(source_file$parsed_content)) return(list())

  xml <- source_file$xml_parsed_content

  library_calls <- c("library", "require", "loadNamespace", "requireNamespace")

  xpath <- sprintf(
    "//expr[expr/SYMBOL_FUNCTION_CALL[%s]]",
    paste0(sprintf("text()='%s'", library_calls), collapse = " or ")
  )

  pkg_calls <- xml2::xml_find_all(xml, xpath)
  pkg_names <- xml2::xml_find_all(pkg_calls,
    "OP-LEFT-PAREN/following-sibling::expr[1]/*[self::SYMBOL | self::STR_CONST]")
  pkg_names <- xml2::xml_text(pkg_names)
  pkg_names <- parse(text = pkg_names, keep.source = FALSE)
  pkg_names <- vapply(pkg_names, format, character(1L))

  installed_packges <- .packages(all.available = TRUE)
  missing_ids <- which(!(pkg_names %in% installed_packges))

  line1 <- as.integer(xml2::xml_attr(pkg_calls, "line1"))
  col1 <- as.integer(xml2::xml_attr(pkg_calls, "col1"))
  col2 <- as.integer(xml2::xml_attr(pkg_calls, "col2"))

  lapply(missing_ids, function(i) {
    Lint(
      filename = source_file$filename,
      line_number = line1[[i]],
      column_number = col1[[i]],
      type = "error",
      message = sprintf("Package '%s' is not installed.", pkg_names[[i]]),
      line = source_file$lines[line1[[i]]],
      ranges = list(c(col1[[i]], col2[[i]])),
      linter = "missing_pkg_linter"
    )
  })
}
