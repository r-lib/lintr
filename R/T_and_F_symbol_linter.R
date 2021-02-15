#' @include aaa.R
#'
#' @describeIn linters  Avoid the symbols \code{T} and \code{F} (for \code{TRUE} and \code{FALSE}).
#' @export
T_and_F_symbol_linter <- function() { # nolint: object_name_linter.
  Linter(function(source_file) {
    if (is.null(source_file$xml_parsed_content)) return(list())

    xpath <- paste0(
      "//SYMBOL[",
      "(text() = 'T' or text() = 'F')", # T or F symbol
      " and count(preceding-sibling::OP-DOLLAR) = 0", # not part of a $-subset expression
      " and count(parent::expr/following-sibling::LEFT_ASSIGN) = 0", # not target of left assignment
      " and count(parent::expr/preceding-sibling::RIGHT_ASSIGN) = 0", # not target of right assignment
      " and count(parent::expr/following-sibling::EQ_ASSIGN) = 0", # not target of equals assignment
      "]"
    )

    bad_exprs <- xml2::xml_find_all(source_file$xml_parsed_content, xpath)

    lapply(bad_exprs, function(expr) {
      symbol <- xml2::xml_text(expr)
      replacement <- switch(symbol, "T" = "TRUE", "F" = "FALSE")
      message <- sprintf("Use %s instead of the symbol %s.", replacement, symbol)
      xml_nodes_to_lint(
        xml = expr,
        source_file = source_file,
        message = message,
        linter = "T_and_F_symbol_linter",
        type = "style",
        offset = 1L
      )
    })
  })
}
