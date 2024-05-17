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
  pipe_node <- glue("SPECIAL[{ xp_text_in_table(magrittr_pipes) }]")
  preceding_pipe <- glue("preceding-sibling::expr[1]/descendant::*[self::{pipe_node} or self::PIPE]")
  xpath <- glue("
  (//PIPE | //{pipe_node})[
    parent::expr[@line1 < @line2]
    and {preceding_pipe}
    and (
      preceding-sibling::expr[1]/descendant-or-self::expr/@line2
      = following-sibling::expr[1]/descendant-or-self::expr/@line1
      or @line1 = {preceding_pipe}/@line1
    )
  ]
  ")

  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content

    pipe_exprs <- xml_find_all(xml, xpath)
    pipe_text <- xml_text(pipe_exprs)

    xml_nodes_to_lints(
      pipe_exprs,
      source_expression = source_expression,
      lint_message = sprintf(
        "Put a space before `%s` and a new line after it, unless the full pipeline fits on one line.",
        pipe_text
      ),
      type = "style"
    )
  })
}
