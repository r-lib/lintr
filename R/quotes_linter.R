#' Character string quote linter
#'
#' Check that the desired quote delimiter is used for string constants.
#'
#' @param delimiter Which quote delimiter to accept. Defaults to the tidyverse
#'   default of `"` (double-quoted strings).
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "c('a', 'b')",
#'   linters = quotes_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'c("a", "b")',
#'   linters = quotes_linter()
#' )
#'
#' code_lines <- "paste0(x, '\"this is fine\"')"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = quotes_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "c('a', 'b')",
#'   linters = quotes_linter(delimiter = "'")
#' )
#'
#' @evalRd rd_tags("quotes_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#character-vectors>
#' @export
quotes_linter <- function(delimiter = c('"', "'")) {
  delimiter <- match.arg(delimiter)
  if (delimiter == '"') {
    quote_regex <- rex(
      start,
      zero_or_one(character_class("rR")),
      single_quote,
      any_non_double_quotes,
      single_quote,
      end
    )
    lint_message <- "Only use double-quotes." # nolint: object_usage. An apparent codetools bug.
  } else {
    quote_regex <- rex(
      start,
      zero_or_one(character_class("rR")),
      double_quote,
      any_non_single_quotes,
      double_quote,
      end
    )
    lint_message <- "Only use single-quotes."
  }

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    content <- source_expression$full_parsed_content
    str_idx <- which(content$token == "STR_CONST")
    quote_matches <- which(re_matches(content[str_idx, "text"], quote_regex))

    lapply(
      quote_matches,
      function(id) {
        with(content[str_idx[id], ], {
          line <- source_expression$file_lines[[line1]] # nolint: object_usage. Codetools bug
          col2 <- if (line1 == line2) col2 else nchar(line) # nolint: object_usage. Codetools bug
          Lint(
            filename = source_expression$filename,
            line_number = line1,
            column_number = col1,
            type = "style",
            message = lint_message,
            line = line,
            ranges = list(c(col1, col2))
          )
        })
      }
    )
  })
}
