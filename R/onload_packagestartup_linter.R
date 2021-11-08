#' @describeIn linters `packageStartupMessage()` should be used inside of a package's [.onAttach()],
#'    hook, *not* [.onLoad()]. See the "Good practice" section in ?".onLoad".
#' @export
onload_packagestartup_linter <- function(source_file) {
  if (length(source_file$parsed_content) == 0L) {
    return(list())
  }

  xml <- source_file$xml_parsed_content

  xpath <- sprintf(
    "//expr[%s]/following-sibling::expr[FUNCTION]//%s",
    "SYMBOL[text() = '.onLoad']",
    "SYMBOL_FUNCTION_CALL[text() = 'packageStartupMessage']"
  )

  bad_expr <- xml2::xml_find_all(xml, xpath)

  return(lapply(
    bad_expr,
    xml_nodes_to_lint,
    source_file = source_file,
    message =
      "Put packageStartupMessage() calls in .onAttach(), not .onLoad().",
    linter = "OnLoadPackageStartupMessageLinter",
    type = "warning"
  ))
}

