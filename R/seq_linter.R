#' Sequence linter
#'
#' This linter checks for `1:length(...)`, `1:nrow(...)`, `1:ncol(...)`,
#' `1:NROW(...)` and `1:NCOL(...)` expressions in base-R, or their usage in
#' conjunction with `seq()` (e.g., `seq(length(...))`, `seq(nrow(...))`, etc.).
#'
#' Additionally, it checks for `1:n()` (from `{dplyr}`) and `1:.N` (from `{data.table}`).
#'
#' These often cause bugs when the right-hand side is zero.
#' It is safer to use [base::seq_len()] or [base::seq_along()] instead.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "seq(length(x))",
#'   linters = seq_linter()
#' )
#'
#' lint(
#'   text = "1:nrow(x)",
#'   linters = seq_linter()
#' )
#'
#' lint(
#'   text = "dplyr::mutate(x, .id = 1:n())",
#'   linters = seq_linter()
#' )
#'
#' lint(
#'   text = "unlist(lapply(x, seq_len))",
#'   linters = seq_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "seq_along(x)",
#'   linters = seq_linter()
#' )
#'
#' lint(
#'   text = "seq_len(nrow(x))",
#'   linters = seq_linter()
#' )
#'
#' lint(
#'   text = "dplyr::mutate(x, .id = seq_len(n()))",
#'   linters = seq_linter()
#' )
#'
#' lint(
#'   text = "sapply(x, seq_len)",
#'   linters = seq_linter()
#' )
#'
#' @evalRd rd_tags("seq_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
seq_linter <- function() {
  bad_funcs <- xp_text_in_table(c("length", "n", "nrow", "ncol", "NROW", "NCOL", "dim"))

  # Exact `xpath` depends on whether bad function was used in conjunction with `seq()`
  seq_xpath <- glue("
  parent::expr
    /following-sibling::expr[1][expr/SYMBOL_FUNCTION_CALL[ {bad_funcs} ]]
    /parent::expr[count(expr) = 2]
  ")
  # `.N` from {data.table} is special since it's not a function but a symbol
  colon_xpath <- glue("
  //OP-COLON
    /parent::expr[
      expr[NUM_CONST[text() = '1' or text() = '1L']]
      and (
        expr[expr[(expr|self::*)[SYMBOL_FUNCTION_CALL[ {bad_funcs} ]]]]
        or expr[SYMBOL = '.N']
      )
    ]
  ")

  map_funcs <- c("sapply", "lapply", "map")
  seq_funcs <- xp_text_in_table(c("seq_len", "seq"))
  sequence_xpath <- glue("
    parent::expr[following-sibling::expr/SYMBOL[ {seq_funcs} ]]
    /parent::expr[preceding-sibling::expr/SYMBOL_FUNCTION_CALL[text() = 'unlist']]"
  )

  ## The actual order of the nodes is document order
  ## In practice we need to handle length(x):1
  get_fun <- function(expr, n) {
    funcall <- xml_find_chr(expr, sprintf("string(./expr[%d])", n))

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

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    seq_calls <- source_expression$xml_find_function_calls("seq")

    badx <- combine_nodesets(
      xml_find_all(seq_calls, seq_xpath),
      xml_find_all(xml, colon_xpath)
    )

    dot_expr1 <- get_fun(badx, 1L)
    dot_expr2 <- get_fun(badx, 2L)
    seq_along_idx <- grepl("length(", dot_expr1, fixed = TRUE) | grepl("length(", dot_expr2, fixed = TRUE)
    rev_idx <- startsWith(dot_expr2, "1")

    replacement <- rep("seq_along(...)", length(badx))
    replacement[!seq_along_idx] <- paste0("seq_len(", ifelse(rev_idx, dot_expr1, dot_expr2)[!seq_along_idx], ")")
    replacement[rev_idx] <- paste0("rev(", replacement[rev_idx], ")")

    lint_message <- ifelse(
      grepl("seq", dot_expr1, fixed = TRUE),
      sprintf(
        "Use %s instead of %s(%s), which is likely to be wrong in the empty edge case.",
        replacement, dot_expr1, dot_expr2
      ),
      sprintf(
        "Use %s instead of %s:%s, which is likely to be wrong in the empty edge case.",
        replacement, dot_expr1, dot_expr2
      )
    )

    seq_lints <- xml_nodes_to_lints(badx, source_expression, lint_message, type = "warning")

    xml_map_calls <- source_expression$xml_find_function_calls(map_funcs)
    potential_sequence_calls <- xml_find_all(xml_map_calls, sequence_xpath)
    sequence_lints <- xml_nodes_to_lints(
      potential_sequence_calls,
      source_expression,
      "Use sequence() to generate a concatenated sequence of seq_len().",
      type = "warning"
    )

    c(seq_lints, sequence_lints)
  })
}
