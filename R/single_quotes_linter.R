#' Single quotes linter
#'
#' Check that only double quotes are used to delimit string constants.
#'
#' @evalRd rd_tags("single_quotes_linter")
#' @seealso
#'   [linters] for a complete list of linters available in lintr. \cr
#'   <https://style.tidyverse.org/syntax.html#character-vectors>
#' @export
single_quotes_linter <- function() {
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    content <- source_expression$full_parsed_content
    str_idx <- which(content$token == "STR_CONST")
    squote_matches <- which(re_matches(
      content[str_idx, "text"],
      rex(start, zero_or_one(character_class("rR")), single_quote, any_non_double_quotes, single_quote, end)
    ))

    lapply(
      squote_matches,
      function(id) {
        with(content[str_idx[id], ], {
          line <- source_expression$file_lines[[line1]]
          col2 <- if (line1 == line2) col2 else nchar(line)
          Lint(
            filename = source_expression$filename,
            line_number = line1,
            column_number = col1,
            type = "style",
            message = "Only use double-quotes.",
            line = line,
            ranges = list(c(col1, col2))
          )
        })
      }
    )
  })
}
