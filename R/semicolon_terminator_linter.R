#' @describeIn linters  Check that no semicolons terminate statements.
#' @param type A character vector defining which semicolons to report:\describe{
#'   \item{compound}{Semicolons that separate two statements on the same line.}
#'   \item{trailing}{Semicolons following the last statement on the line.}
#' }
#' @export
semicolon_terminator_linter <- function(type = c("compound", "trailing")) {
  function(source_file) {
    sc_ids <- ids_with_token(source_file, "';'")
    if (length(sc_ids)) {
      sc_tokens <- with_id(source_file, sc_ids)
      is_trailing <- is_trailing_sc(sc_tokens, source_file)

      to_keep <- ( is_trailing & "trailing" %in% type) |
                 (!is_trailing & "compound" %in% type)
 
      sc_tokens <- sc_tokens[to_keep, ]
      is_trailing <- is_trailing[to_keep]

      lapply(
        seq_len(nrow(sc_tokens)),
        function(sc_num) {
          sc_token <- sc_tokens[sc_num, ]
          msg <- if (is_trailing[[sc_num]]) {
            "Trailing semicolons are not needed."
          } else  {
            "Compound semicolons are not needed. Replace them by a newline."
          }
          Lint(
            filename = source_file[["filename"]],
            line_number = sc_token[["line1"]],
            column_number = sc_token[["col1"]],
            type = "style",
            message = msg,
            line = source_file[["lines"]][[as.character(sc_token[["line1"]])]],
            ranges = list(c(sc_token[["col1"]], sc_token[["col2"]])),
            linter = "semicolon_linter"
          )
        }
      )
    }
  }
}


is_trailing_sc <- function(sc_tokens, source_file) {
  line_str <- source_file[["lines"]][as.character(sc_tokens[["line1"]])]
  tail_str <- substr(line_str, sc_tokens[["col1"]] + 1L, nchar(line_str))
  grepl("^\\s*(#|}|\\z)", tail_str, perl = TRUE)
}

