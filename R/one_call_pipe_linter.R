#' Block single-call magrittr pipes
#'
#' Prefer using a plain call instead of a pipe with only one call,
#'   i.e. `1:10 %>% sum()` should instead be `sum(1:10)`. Note that
#'   calls in the first `%>%` argument count. `rowSums(x) %>% max()` is OK
#'   because there are two total calls (`rowSums()` and `max()`).
#'
#' Note also that un-"called" steps are *not* counted, since they should
#'   be calls (see [pipe_call_linter()]).
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "(1:10) %>% sum()",
#'   linters = one_call_pipe_linter()
#' )
#'
#' lint(
#'   text = "DT %>% .[grp == 'a', sum(v)]",
#'   linters = one_call_pipe_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "rowSums(x) %>% mean()",
#'   linters = one_call_pipe_linter()
#' )
#'
#' lint(
#'   text = "DT[src == 'a', .N, by = grp] %>% .[N > 10]",
#'   linters = one_call_pipe_linter()
#' )
#'
#' # assignment pipe is exempted
#' lint(
#'   text = "DF %<>% mutate(a = 2)",
#'   linters = one_call_pipe_linter()
#' )
#'
#' @evalRd rd_tags("one_call_pipe_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/pipes.html#short-pipes>
#' @export
one_call_pipe_linter <- function() {
  # exception for assignment pipe per #2330
  pipes_cond <- xp_text_in_table(setdiff(magrittr_pipes, "%<>%"))

  # preceding-sibling::SPECIAL: if there are ever two pipes, don't lint
  # OP-LEFT-BRACKET/LBB: accept DT[...] %>% .[...] as a two-call pipe,
  #   (but not DT %>% .[...])
  # parent::expr/SPECIAL: make sure we are at the top of a pipeline
  # count(): any call anywhere else in the AST within the pipe expression
  xpath <- glue("
  (//SPECIAL[{pipes_cond}] | //PIPE)[
    not(preceding-sibling::expr[1]/*[self::SPECIAL[{pipes_cond}] or self::PIPE])
    and (
      not(following-sibling::expr[OP-LEFT-BRACKET or LBB])
      or not(preceding-sibling::expr[OP-LEFT-BRACKET or LBB])
    )
  ]
    /parent::expr[
      not(parent::expr/*[self::SPECIAL[{ pipes_cond }] or self::PIPE])
      and count(.//SYMBOL_FUNCTION_CALL) <= 1
    ]
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)
    pipe <- xml_find_chr(bad_expr, "string(SPECIAL | PIPE)")

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste0("Avoid pipe ", pipe, " for expressions with only a single call."),
      type = "warning"
    )
  })
}
