#' Semicolon linter
#'
#' Check that no semicolons terminate expressions.
#'
#' @param allow_compound Logical, default `FALSE`. If `TRUE`, "compound"
#'   semicolons (e.g. as in `x; y`, i.e., on the same line of code) are allowed.
#' @param allow_trailing Logical, default `FALSE`. If `TRUE`, "trailing"
#'   semicolons (i.e., those that terminate lines of code) are allowed.
#' @evalRd rd_tags("semicolon_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#semicolons>
#' @export
semicolon_linter <- function(allow_compound = FALSE, allow_trailing = FALSE) {
  Linter(function(source_file) {
    tokens <- with_id(source_file, ids_with_token(source_file, "';'"))
    is_trailing <- is_trailing_sc(tokens, source_file)

    to_keep <- (is_trailing & !allow_trailing) |
               (!is_trailing & !allow_compound)

    tokens <- tokens[to_keep, ]
    are_trailing <- is_trailing[to_keep]

    Map(
      function(token, is_trailing) {
        msg <- if (is_trailing) {
          "Trailing semicolons are not needed."
        } else  {
          "Compound semicolons are discouraged. Replace them by a newline."
        }

        Lint(
          filename = source_file[["filename"]],
          line_number = token[["line1"]],
          column_number = token[["col1"]],
          type = "style",
          message = msg,
          line = source_file[["lines"]][[as.character(token[["line1"]])]],
          ranges = list(c(token[["col1"]], token[["col2"]]))
        )
      },
      split(tokens, seq_len(nrow(tokens))),
      are_trailing
    )
  })
}

#' @rdname semicolon_linter
#' @param semicolon A character vector defining which semicolons to report:
#' \describe{
#'   \item{compound}{Semicolons that separate two statements on the same line.}
#'   \item{trailing}{Semicolons following the last statement on the line.}
#' }
#' @export
semicolon_terminator_linter <- function(semicolon = c("compound", "trailing")) {
  lintr_deprecated(old = "semicolon_terminator_linter", new = "semicolon_linter", version = "2.0.1.9001", type = "Linter")
  semicolon <- match.arg(semicolon, several.ok = TRUE)
  allow_compound <- !"compound" %in% semicolon
  allow_trailing <- !"trailing" %in% semicolon
  semicolon_linter(allow_compound, allow_trailing)
}

is_trailing_sc <- function(sc_tokens, source_file) {
  line_str <- source_file[["lines"]][as.character(sc_tokens[["line1"]])]
  tail_str <- substr(line_str, sc_tokens[["col1"]] + 1L, nchar(line_str))
  grepl("^\\s*(#|}|\\z)", tail_str, perl = TRUE)
}
