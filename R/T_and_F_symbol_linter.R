#' @include aaa.R
#'
#' @describeIn linters  Avoid the symbols \code{T} and \code{F} (for \code{TRUE} and \code{FALSE}).
#' @export
T_and_F_symbol_linter <- function(source_file) {
  lapply(
    ids_with_token(source_file, "SYMBOL"),
    function(id) {
      token <- with_id(source_file, id)
      symbol <- re_matches(token[["text"]], rex(start, capture(or("T", "F")), end))[1L, 1L]
      if (!is.na(symbol)) {
        replacement <- switch(symbol, "T"="TRUE", "F"="FALSE")
        line_num <- token[["line2"]]
        start_col_num <- token[["col1"]]
        end_col_num <- token[["col2"]]
        Lint(
          filename = source_file[["filename"]],
          line_number = line_num,
          column_number = end_col_num + 1L,
          type = "style",
          message = sprintf("Use %s instead of the symbol %s.", replacement, symbol),
          line = source_file[["lines"]][[as.character(line_num)]],
          ranges = list(c(start_col_num, end_col_num)),
          linter = "T_and_F_symbol_linter"
        )
      }
    }
  )
}
