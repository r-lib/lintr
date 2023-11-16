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
#' @evalRd rd_tags("one_call_pipe_linter")
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/pipes.html#short-pipes>
#' @export
one_call_pipe_linter <- function() {
  pipes_cond <- xp_text_in_table(c("%>%", "%$%", "%T>%"))

  # preceding-sibling::SPECIAL: if there are ever two pipes, don't lint
  # OP-LEFT-BRACKET/LBB: accept DT[...] %>% .[...] as a two-call pipe,
  #   (but not DT %>% .[...])
  # parent::expr/SPECIAL: make sure we are at the top of a pipeline
  # count(): any call anywhere else in the AST within the pipe expression
  # TODO(michaelchirico): Add support for native pipe |> like DT |> _[...]
  xpath <- glue("
  //SPECIAL[
    ({ pipes_cond })
    and not(preceding-sibling::expr[1]/SPECIAL[{ xp_text_in_table(magrittr_pipes) }])
    and (
      not(following-sibling::expr[OP-LEFT-BRACKET or LBB])
      or not(preceding-sibling::expr[OP-LEFT-BRACKET or LBB])
    )
  ]
    /parent::expr[
      not(parent::expr/SPECIAL[{ pipes_cond }])
      and count(.//SYMBOL_FUNCTION_CALL) <= 1
    ]
  |
  //PIPE[not(preceding-sibling::expr[1]/PIPE)]
    /parent::expr[
      not(parent::expr/PIPE)
      and count(.//SYMBOL_FUNCTION_CALL) <= 1
    ]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)
    pipe <- xml_find_chr(bad_expr, "string(SPECIAL | PIPE)")

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste0("Expressions with only a single call shouldn't use pipe ", pipe, "."),
      type = "warning"
    )
  })
}
