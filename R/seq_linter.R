#' Sequence linter
#'
#' This linter checks for expressions like `1:length(...)` (and many other spiritually
#'   equivalent variations) which are a common source of bugs when the right-hand side is zero.
#'   It is safer to use [base::seq_len()] (to create a sequence of a specified *length*) or
#'   [base::seq_along()] (to create a sequence *along* an object).
#'
#' Additionally, it checks for `1:n()` (from `{dplyr}`) and `1:.N` (from `{data.table}`).
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "seq(length(x))",
#'   linters = seq_linter()
#' )
#'
#' lint(
#'   text = "seq(1, 10)",
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
#'   text = "seq_len(length(x))",
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
#'   text = "seq_len(10)",
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
#'   text = "seq_along(x)",
#'   linters = seq_linter()
#' )
#'
#' lint(
#'   text = "sequence(x)",
#'   linters = seq_linter()
#' )
#'
#' @evalRd rd_tags("seq_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
seq_linter <- function() {
  bad_funcs <- xp_text_in_table(c("length", "n", "nrow", "ncol", "NROW", "NCOL", "dim"))

  # Exact `xpath` depends on whether bad function was used in conjunction with `seq()`
  # or if seq() is called with 2 arguments (from = 1, to = n)
  seq_xpath <- glue("
  parent::expr[
    (
      count(expr) = 2
      and expr[2][expr/SYMBOL_FUNCTION_CALL[ {bad_funcs} ]]
    )
    or (
      count(expr) = 3
      and not(SYMBOL_SUB[text() != 'from' and text() != 'to'])
      and (
        (
          expr[2][NUM_CONST[text() = '1' or text() = '1L']]
          and not(expr[2]/preceding-sibling::SYMBOL_SUB[1][text() = 'to'])
        )
        or (
          expr[3][NUM_CONST[text() = '1' or text() = '1L']]
          and expr[2]/preceding-sibling::SYMBOL_SUB[1][text() = 'to']
        )
      )
    )
  ]
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

  seq_len_xpath <- "
    parent::expr[expr/expr/SYMBOL_FUNCTION_CALL[text() = 'length']]
  "

  map_funcs <- c("sapply", "lapply", "map")
  seq_funcs <- xp_text_in_table(c("seq_len", "seq"))
  # count(expr) = 3 because we only want seq() calls without extra arguments
  sequence_xpath <- glue("
    parent::expr[
      count(expr) = 3
      and expr/SYMBOL[ {seq_funcs} ]
      and preceding-sibling::expr/SYMBOL_FUNCTION_CALL[text() = 'unlist']
    ]
  ")

  format_arg <- function(funcalls) {
    funcalls_clean <- trimws(funcalls)
    # `dplyr::n()` is special because it has no arguments, so the lint message
    # should mention `n()`, and not `n(...)`
    has_paren <- grepl("(", funcalls_clean, fixed = TRUE) & funcalls_clean != "n()"
    funcalls_clean[has_paren] <- sub("\\(.*\\)", "(...)", funcalls_clean[has_paren])
    funcalls_clean
  }

  get_seq_metadata <- function(seq_expr) {
    if (length(seq_expr) == 0L) {
      return(data.frame(
        is_seq = logical(0L),
        dot_expr1 = character(0L),
        dot_expr2 = character(0L),
        replacement = character(0L),
        lint_message = character(0L)
      ))
    }

    is_seq <- is.na(xml_find_first_(seq_expr, "./OP-COLON"))

    expr_counts <- as.integer(xml_find_chr_(seq_expr, "string(count(./expr))"))
    expr1_text <- xml_find_chr_(seq_expr, "string(./expr[1])")
    expr2_text <- xml_find_chr_(seq_expr, "string(./expr[2])")
    expr3_text <- xml_find_chr_(seq_expr, "string(./expr[3])")

    is_expr2_to <-
      !is.na(xml_text(xml_find_first_(seq_expr, "./expr[2]/preceding-sibling::SYMBOL_SUB[1][text() = 'to']")))

    raw_expr1 <- character(length(seq_expr))
    raw_expr2 <- character(length(seq_expr))

    is_1arg_seq <- is_seq & expr_counts == 2L
    is_2arg_seq <- is_seq & expr_counts != 2L

    raw_expr1[!is_seq] <- expr1_text[!is_seq]
    raw_expr2[!is_seq] <- expr2_text[!is_seq]

    raw_expr1[is_1arg_seq] <- "seq"
    raw_expr2[is_1arg_seq] <- expr2_text[is_1arg_seq]

    raw_expr1[is_2arg_seq] <- ifelse(is_expr2_to[is_2arg_seq], expr3_text[is_2arg_seq], expr2_text[is_2arg_seq])
    raw_expr2[is_2arg_seq] <- ifelse(is_expr2_to[is_2arg_seq], expr2_text[is_2arg_seq], expr3_text[is_2arg_seq])

    dot_expr1 <- format_arg(raw_expr1)
    dot_expr2 <- format_arg(raw_expr2)

    seq_along_idx <- dot_expr1 == "length(...)" | dot_expr2 == "length(...)"
    rev_idx <- dot_expr2 %in% c("1", "1L")

    replacement <- rep("seq_along(...)", length(seq_expr))
    replacement[!seq_along_idx] <- paste0("seq_len(", ifelse(rev_idx, dot_expr1, dot_expr2)[!seq_along_idx], ")")
    replacement[rev_idx] <- paste0("rev(", replacement[rev_idx], ")")

    seq_call <- ifelse(
      dot_expr1 == "seq",
      paste0("seq(", dot_expr2, ")"),
      paste0("seq(", dot_expr1, ", ", dot_expr2, ")")
    )
    colon_call <- paste0(dot_expr1, ":", dot_expr2)
    got_expr <- ifelse(is_seq, seq_call, colon_call)

    lint_message <- sprintf(
      "Use %s instead of %s, which is likely to be wrong in the empty edge case.",
      replacement, got_expr
    )

    data.frame(
      is_seq = is_seq,
      dot_expr1 = dot_expr1,
      dot_expr2 = dot_expr2,
      replacement = replacement,
      lint_message = lint_message,
      stringsAsFactors = FALSE
    )
  }

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    seq_calls <- source_expression$xml_find_function_calls("seq")

    seq_expr <- combine_nodesets(
      xml_find_all_(seq_calls, seq_xpath),
      xml_find_all_(xml, colon_xpath)
    )
    seq_expr <- strip_comments_from_subtree(seq_expr)

    seq_meta <- get_seq_metadata(seq_expr)

    seq_lints <- xml_nodes_to_lints(seq_expr, source_expression, seq_meta$lint_message, type = "warning")

    seq_len_calls <- source_expression$xml_find_function_calls("seq_len")
    seq_len_expr <- xml_find_all_(seq_len_calls, seq_len_xpath)
    seq_len_lints <- xml_nodes_to_lints(
      seq_len_expr,
      source_expression,
      "Use seq_along(x) instead of seq_len(length(x)).",
      type = "warning"
    )

    xml_map_calls <- source_expression$xml_find_function_calls(map_funcs)
    potential_sequence_calls <- xml_find_all_(xml_map_calls, sequence_xpath)
    sequence_lints <- xml_nodes_to_lints(
      potential_sequence_calls,
      source_expression,
      "Use sequence() to generate a concatenated sequence of seq_len().",
      type = "warning"
    )

    c(seq_lints, seq_len_lints, sequence_lints)
  })
}
