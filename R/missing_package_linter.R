#' @describeIn linters checks for missing packages in \code{library()},
#'   \code{require()}, \code{loadNamespace()} and \code{requireNamespace()} calls.
#' @export
missing_package_linter <- function() {
  Linter(function(source_file) {

    if (is.null(source_file$full_xml_parsed_content)) return(list())

    xml <- source_file$full_xml_parsed_content

    library_calls <- c("library", "require", "loadNamespace", "requireNamespace")

    name_xpath <- "OP-LEFT-PAREN[1]/following-sibling::expr[1][SYMBOL | STR_CONST]"
    call_xpath <- paste0(sprintf(
      "//expr[expr[SYMBOL_FUNCTION_CALL[%s]]/following-sibling::%s]",
      paste0(sprintf("text()='%s'", library_calls), collapse = " or "),
      name_xpath
    ))

    pkg_calls <- xml2::xml_find_all(xml, call_xpath)
    pkg_names <- xml2::xml_find_all(pkg_calls, name_xpath)
    pkg_names <- xml2::xml_text(pkg_names)
    pkg_names <- parse(text = pkg_names, keep.source = FALSE)
    pkg_names <- vapply(pkg_names, as.character, character(1L))

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
        type = "warning",
        message = sprintf("Package '%s' is not installed.", pkg_names[[i]]),
        line = source_file$file_lines[line1[[i]]],
        ranges = list(c(col1[[i]], col2[[i]]))
      )
    })
  })
}
