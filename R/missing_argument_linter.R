#' @describeIn linters checks for missing arguments in function calls.
#' @export
missing_argument_linter <- function(source_file) {

  if (!length(source_file$parsed_content) || is.null(source_file$xml_parsed_content)) return(list())

  xml <- source_file$xml_parsed_content

  xpath <- "//expr[expr[SYMBOL_FUNCTION_CALL]]/*[
    self::OP-COMMA[preceding-sibling::*[1][self::OP-LEFT-PAREN or self::OP-COMMA]] or
    self::OP-COMMA[following-sibling::*[1][self::OP-RIGHT-PAREN]] or
    self::EQ_SUB[following-sibling::*[1][self::OP-RIGHT-PAREN]]
  ]"

  missing_args <- xml2::xml_find_all(xml, xpath)

  line1 <- as.integer(xml2::xml_attr(missing_args, "line1"))
  col1 <- as.integer(xml2::xml_attr(missing_args, "col1"))
  col2 <- as.integer(xml2::xml_attr(missing_args, "col2"))

  lapply(seq_along(missing_args), function(i) {
    Lint(
      filename = source_file$filename,
      line_number = line1[[i]],
      column_number = col1[[i]],
      type = "warning",
      message = "Missing argument in function call.",
      line = source_file$lines[line1[[i]]],
      ranges = list(c(col1[[i]], col2[[i]])),
      linter = "missing_argument_linter"
    )
  })
}
