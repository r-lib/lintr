#' @describeIn linters  Check that no semicolons terminate statements.
#' @param type A character vector defining which semicolons to report:\describe{
#'   \item{compound}{Semicolons that separate two statements on the same line.}
#'   \item{trailing}{Semicolons following the last statement on the line.}
#' }
#' @export
semicolon_terminator_linter <- function(type = c("compound", "trailing")) {
  function(source_file) {
    if (is.null(source_file[["parsed_content"]])) {
      list()
    } else {
      semicolons <- c(locate_tokenized_sc(source_file), locate_other_sc(source_file))
      if (length(type) == 1L) {
        # Keep exclusively compound or trailing semicolons
        keep_trailing <- if (type == "trailing") {TRUE} else {FALSE}
        to_keep <- vapply(semicolons, function(sc) {sc[[3L]] == keep_trailing}, logical(1L))
        semicolons <- semicolons[to_keep]
      }
      lapply(
        semicolons,
        function(line_col_trail) {
          line_num <- line_col_trail[[1L]]
          col_num <- line_col_trail[[2L]]
          is_trailing <- line_col_trail[[3L]]
          msg <- if (is_trailing) {
            "Trailing semicolons are not needed."
          } else {
            "Compound semicolons are not needed. Replace them by a newline."
          }
          Lint(
            filename = source_file[["filename"]],
            line_number = line_num,
            column_number = col_num,
            type = "style",
            message = msg,
            line = source_file[["lines"]][[as.character(line_num)]],
            ranges = list(c(col_num, col_num)),
            linter = "semicolon_linter"
          )
        }
      )
    }
  }
}

locate_tokenized_sc <- function(source_file) {
  # Identify location of semicolons explicitly listed in the tokens, i.e. those in a {} block.
  # For example: "function() {a <- 1;}"
  locs <- which(source_file[["parsed_content"]][["token"]] == "';'")
  if (length(locs)) {
    lapply(
      locs,
      function(loc) {
        token <- source_file[["parsed_content"]][loc, ]
        line_num <- token[["line1"]]
        col_num <- token[["col1"]]
        line_str <- source_file[["lines"]][[as.character(line_num)]]
        tail_str <- substr(line_str, col_num, nchar(line_str))
        is_trailing <- grepl("^\\s*;\\s*(#|}|\\z)", tail_str, perl = TRUE)
        list(line_num, col_num, is_trailing)
      }
    )
  } else {
    list()
  }
}

locate_other_sc <- function(source_file) {
  # Identify location of semicolons not explicitly listed in the tokens, i.e. those not in a {}
  # block. Example: "a <- 1;". To achieve this, look at the string trailing at the end of the
  # given expression and see if it contains a semicolon.
  tokens <- source_file[["parsed_content"]]
  last_token <- tokens[dim(tokens)[[1L]], ]
  line_num <- last_token[["line2"]]
  col_num <- last_token[["col2"]] + 1L
  line_str <- source_file[["lines"]][[as.character(line_num)]]
  tail_str <- substr(line_str, col_num, nchar(line_str))
  match <- regexec("^\\s*;", tail_str)[[1L]]
  if (match > -1L) {
    match_col <- last_token[["col2"]] + attr(match, "match.length")
    is_trailing <- grepl("^\\s*;\\s*(#|}|\\z)", tail_str, perl = TRUE)
    list( list(line_num, match_col, is_trailing) )
  } else {
    list()
  }
}
