#' @describeIn linters Report the use of undesirable functions, e.g.
#'   \code{return}, \code{options}, or \code{sapply} and suggest an alternative.
#' @param fun Named character vector, where the names are the names of the
#'   undesirable functions, and the values are the text for the alternative
#'   function to use (or \code{NA}).
#' @param symbol_is_undesirable Whether to consider the use of an undesirable
#'   function name as a symbol undesirable or not.
#' @export
undesirable_function_linter <- function(fun = default_undesirable_functions,
                                        symbol_is_undesirable = TRUE) {
  stopifnot(is.logical(symbol_is_undesirable))

  Linter(function(source_file) {
    if (is.null(source_file$xml_parsed_content)) return(NULL)
    if (symbol_is_undesirable) {
      tokens <- c("SYMBOL_FUNCTION_CALL", "SYMBOL")
    } else {
      tokens <- "SYMBOL_FUNCTION_CALL"
    }

    xpath <- paste0(
      "//",
      tokens,
      "[",
      paste("text()='", names(fun), "'", sep = "", collapse = " or "),
      " and ",
      "count(parent::expr/preceding-sibling::expr/SYMBOL_FUNCTION_CALL[text()='library' or text()='require'])=0",
      "]",
      collapse = " | "
    )
    matched_nodes <- xml2::xml_find_all(source_file$xml_parsed_content, xpath)

    lapply(
      matched_nodes,
      function(node) {
        fun_name <- as.character(xml2::xml_contents(node))
        msg <- sprintf("Function \"%s\" is undesirable.", fun_name)
        if (!is.na(fun[[fun_name]])) {
          msg <- paste(msg, sprintf("As an alternative, %s.", fun[[fun_name]]))
        }
        line <- as.integer(xml2::xml_attr(node, "line1"))
        col1 <- as.integer(xml2::xml_attr(node, "col1"))
        col2 <- as.integer(xml2::xml_attr(node, "col2"))
        Lint(
          filename = source_file$filename,
          line_number = line,
          column_number = col1,
          type = "style",
          message = msg,
          line = source_file$lines[[line]],
          ranges = list(c(col2, col1))
        )
      }
    )
  })

}
