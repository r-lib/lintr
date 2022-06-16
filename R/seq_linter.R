#' Sequence linter
#'
#' Check for `1:length(...)`, `1:nrow(...)`, `1:ncol(...)`, `1:NROW(...)` and `1:NCOL(...)` expressions in base-R.
#' Additionally check `1:n()` (from dplyr) and `1:.N` (from data.table).
#' These often cause bugs when the right-hand side is zero.
#' It is safer to use [base::seq_len()] or [base::seq_along()] instead.
#'
#' @evalRd rd_tags("seq_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
seq_linter <- function() {
  bad_funcs <- c("length", "n", "nrow", "ncol", "NROW", "NCOL", "dim")

  # `.N` from {data.table} is special since it's not a function but a symbol
  xpath <- glue::glue("//expr[
    expr[NUM_CONST[text() =  '1' or text() =  '1L']]
    and OP-COLON
    and (
          expr[expr[(expr|self::*)[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(bad_funcs)} ]]]]
          or
          expr[SYMBOL = '.N']
        )
  ]")

  ## The actual order of the nodes is document order
  ## In practice we need to handle length(x):1
  get_fun <- function(expr, n) {
    funcall <- xml2::xml_find_chr(expr, sprintf("string(./expr[%d])", n))

    # `dplyr::n()` is special because it has no arguments, so the lint message
    # should mention `n()`, and not `n(...)`
    if (identical(funcall, "n()")) {
      return(funcall)
    }

    fun <- gsub("\\(.*\\)", "(...)", trimws(funcall))
    bad_fun <- fun %in% bad_funcs
    fun[bad_fun] <- paste0(fun[bad_fun], "(...)")
    fun
  }

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    badx <- xml2::xml_find_all(xml, xpath)

    # TODO: better message customization. For example, length(x):1
    #   would get rev(seq_along(x)) as the preferred replacement.
    dot_expr1 <- get_fun(badx, 1L)
    dot_expr2 <- get_fun(badx, 2L)
    replacement <- ifelse(
      grepl("length(", dot_expr1, fixed = TRUE) | grepl("length(", dot_expr2, fixed = TRUE),
      "seq_along",
      "seq_len"
    )
    lint_message <- sprintf(
      "%s:%s is likely to be wrong in the empty edge case. Use %s() instead.",
      dot_expr1, dot_expr2, replacement
    )

    xml_nodes_to_lints(badx, source_expression, lint_message, type = "warning")
  })
}
