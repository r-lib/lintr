#' Pipe call linter
#'
#' Force explicit calls in magrittr pipes, e.g., `1:3 %>% sum()` instead of `1:3 %>% sum`.
#' Note that native pipe always requires a function call, i.e. `1:3 |> sum` will produce an error.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "1:3 %>% mean %>% as.character",
#'   linters = pipe_call_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "1:3 %>% mean() %>% as.character()",
#'   linters = pipe_call_linter()
#' )
#'
#' @evalRd rd_tags("pipe_call_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
pipe_call_linter <- function() {
  # NB: the text() here shows up as %&gt;% but that's not matched, %>% is
  # NB: use *[1][self::SYMBOL] to ensure the first element is SYMBOL, otherwise
  #       we include expressions like x %>% .$col
  pipes <- setdiff(magrittr_pipes, "%$%")
  xpath <- glue("//SPECIAL[{ xp_text_in_table(pipes) }]/following-sibling::expr[*[1][self::SYMBOL]]")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)
    pipe <- xml_text(xml_find_first(bad_expr, "preceding-sibling::SPECIAL[1]"))

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message =
        sprintf("Use explicit calls in magrittr pipes, i.e., `a %1$s foo` should be `a %1$s foo()`.", pipe),
      type = "warning"
    )
  })
}
