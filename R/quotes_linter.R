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
    lint_message <- "Only use double-quotes."
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

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    string_exprs <- xml_find_all(xml, "//STR_CONST")
    is_bad <- re_matches(xml_text(string_exprs), quote_regex)

    xml_nodes_to_lints(string_exprs[is_bad], source_expression, lint_message)
  })
}
