#' Enforce library calls using symbols
#'
#' R packages should be imported as symbols, like `library(lintr)`,
#'   and *not* via strings like `library("lintr", character.only = TRUE)`,
#'   with few exceptions.
#'
#' @evalRd rd_tags("character_only_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
character_only_linter <- function() {
  # STR_CONST: block library|require("..."), i.e., supplying a string literal
  # ancestor::expr[FUNCTION]: Skip usages inside functions a la {knitr}
  direct_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'library' or text() = 'require']
    /parent::expr
    /parent::expr[
      expr[2][STR_CONST]
      or (
        SYMBOL_SUB[text() = 'character.only']
        and not(ancestor::expr[FUNCTION])
      )
    ]
  "

  bad_indirect_funs <- c("do.call", "lapply", "sapply", "map", "walk")
  call_symbol_cond <- "
  SYMBOL[text() = 'library' or text() = 'require']
    or STR_CONST[text() = '\"library\"' or text() = '\"require\"']
  "
  indirect_xpath <- glue("
  //SYMBOL_FUNCTION_CALL[{ xp_text_in_table(bad_indirect_funs) }]
    /parent::expr
    /parent::expr[
      not(ancestor::expr[FUNCTION])
      and expr[{ call_symbol_cond }]
    ]
  ")
  call_symbol_path <- glue("./expr[{call_symbol_cond}]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    direct_expr <- xml_find_all(xml, direct_xpath)
    direct_calls <- xp_call_name(direct_expr)
    character_only <-
      xml_find_first(direct_expr, "./SYMBOL_SUB[text() = 'character.only']")
    direct_msg_fmt <- ifelse(
      is.na(character_only),
      "Use symbols, not strings, in %s calls.",
      "Use symbols in %s calls to avoid the need for 'character.only'."
    )
    direct_msg <- sprintf(as.character(direct_msg_fmt), direct_calls)
    direct_lints <- xml_nodes_to_lints(
      direct_expr,
      source_expression = source_expression,
      lint_message = direct_msg,
      type = "warning"
    )

    indirect_expr <- xml_find_all(xml, indirect_xpath)
    indirect_lib_calls <- get_r_string(indirect_expr, call_symbol_path)
    indirect_loop_calls <- xp_call_name(indirect_expr)
    indirect_msg <- sprintf(
      "Call %s() directly, not vectorized with %s().",
      indirect_lib_calls, indirect_loop_calls
    )
    indirect_lints <- xml_nodes_to_lints(
      indirect_expr,
      source_expression = source_expression,
      lint_message = indirect_msg,
      type = "warning"
    )

    c(direct_lints, indirect_lints)
  })
}
