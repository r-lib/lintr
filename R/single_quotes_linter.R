#' @describeIn linters checks that only single quotes are used to delimit
#' string constants.
#' @export
single_quotes_linter <- function(source_file) {
  if (!length(source_file$parsed_content)) return(list())

  xpath <- paste(
    "//STR_CONST[starts-with(text(), \"'\")",
    "and not(contains(text(), \'\"\'))]"
  )
  squote_str <- xml2::xml_find_all(source_file$xml_parsed_content, xpath)
  lapply(squote_str, function(node) {
    attrs <- xml2::xml_attrs(node)
    line1 <- attrs["line1"]
    line1_src <- source_file$lines[line1]
    col1 <- as.integer(attrs["col1"])
    col2 <- if (line1 == attrs["line2"]) {
      as.integer(attrs["col2"])
    } else {
      nchar(line1_src)
    }
    Lint(
      filename = source_file$filename,
      line_number = as.integer(line1),
      column_number = col1,
      type = "style",
      message = "Only use double-quotes.",
      line = line1_src,
      ranges = list(c(col1, col2)),
      linter = "single_quotes_linter"
    )
  })
}
