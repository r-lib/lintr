#' Pipe continuation linter
#'
#' Check that each step in a pipeline is on a new line, or the entire pipe fits on one line.
#'
#' @evalRd rd_tags("pipe_continuation_linter")
#'
#' @examples
#' # will produce lints
#' code_lines <- "1:3 %>%\n mean() %>% as.character()"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = pipe_continuation_linter()
#' )
#'
#' code_lines <- "1:3 |> mean() |>\n as.character()"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = pipe_continuation_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "1:3 %>% mean() %>% as.character()",
#'   linters = pipe_continuation_linter()
#' )
#'
#' code_lines <- "1:3 %>%\n mean() %>%\n as.character()"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = pipe_continuation_linter()
#' )
#'
#' lint(
#'   text = "1:3 |> mean() |> as.character()",
#'   linters = pipe_continuation_linter()
#' )
#'
#' code_lines <- "1:3 |>\n mean() |>\n as.character()"
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = pipe_continuation_linter()
#' )
#'
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/pipes.html#long-lines-2>
#' @export
pipe_continuation_linter <- function() {
  # Where a single-line pipeline is nested inside a larger expression
  #   e.g. inside a function definition), the outer expression can span multiple lines
  #   without throwing a lint.

  pipe_conditions <- "
    parent::expr[@line1 < @line2]
    and preceding-sibling::expr/descendant-or-self::*[self::SPECIAL[text() = '%>%'] or self::PIPE]
    and (
      preceding-sibling::expr/descendant-or-self::expr/@line2
      = following-sibling::expr/descendant-or-self::expr/@line1
      or @line1 = preceding-sibling::expr/descendant-or-self::*[self::SPECIAL[text() = '%>%'] or self::PIPE]/@line1
    )
  "
  xpath <- glue::glue("
  //SPECIAL[text() = '%>%' and {pipe_conditions} ]
  | //PIPE[ {pipe_conditions} ]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }
    xml <- source_expression$full_xml_parsed_content

    pipe_exprs <- xml2::xml_find_all(xml, xpath)
    pipe_text <- ifelse(xml2::xml_name(pipe_exprs) == "PIPE", "|>", "%>%")

    xml_nodes_to_lints(
      pipe_exprs,
      source_expression = source_expression,
      lint_message = sprintf(
        "`%s` should always have a space before it and a new line after it, unless the full pipeline fits on one line.",
        pipe_text
      ),
      type = "style"
    )
  })
}
