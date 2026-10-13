#' Require usage of boolean operators over equivalent arithmetic
#'
#' `length(which(x == y)) == 0` is the same as `!any(x == y)`, but the latter
#'   is more readable and more efficient. Similarly, `sum(x == y) == length(x)`
#'   is the same as `all(x == y)`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "length(which(x == y)) == 0L",
#'   linters = boolean_arithmetic_linter()
#' )
#'
#' lint(
#'   text = "sum(grepl(pattern, x)) == 0",
#'   linters = boolean_arithmetic_linter()
#' )
#'
#' lint(
#'   text = "sum(x == y) == length(x)",
#'   linters = boolean_arithmetic_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "!any(x == y)",
#'   linters = boolean_arithmetic_linter()
#' )
#'
#' lint(
#'   text = "!any(grepl(pattern, x))",
#'   linters = boolean_arithmetic_linter()
#' )
#'
#' lint(
#'   text = "all(x == y)",
#'   linters = boolean_arithmetic_linter()
#' )
#'
#' @evalRd rd_tags("boolean_arithmetic_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
boolean_arithmetic_linter <- function() {
  zero_expr <- "(EQ or NE or GT or LE) and expr/NUM_CONST[text() = '0' or text() = '0L']"
  one_expr <- "(LT or GE) and expr/NUM_CONST[text() = '1' or text() = '1L']"
  any_parent_cond <- glue("parent::expr[({zero_expr}) or ({one_expr})]")

  other_length_arg <- "
    (
      following-sibling::expr[1][parent::expr[EQ or NE or LT or GE]]
      | preceding-sibling::expr[1][parent::expr[EQ or NE or GT or LE]]
    )[expr[1]/SYMBOL_FUNCTION_CALL[text() = 'length']]/expr[2]
  "
  candidate_vec <- function(base) {
    glue("
      (
        {base}[not(expr[1]/SYMBOL_FUNCTION_CALL)]
        | {base}/expr[
          not(self::expr[1][SYMBOL_FUNCTION_CALL])
          and not(preceding-sibling::expr[1]/SYMBOL_FUNCTION_CALL[text() = 'grepl' or text() = 'grep'])
        ]
        | {base}/expr/expr[
          not(self::expr[1][SYMBOL_FUNCTION_CALL])
          and not(preceding-sibling::expr[1]/SYMBOL_FUNCTION_CALL[text() = 'grepl' or text() = 'grep'])
        ]
      )
    ")
  }

  length_any_xpath <- glue("
  parent::expr
    /parent::expr[
      expr/SYMBOL_FUNCTION_CALL[text() = 'length']
      and {any_parent_cond}
    ]
  ")
  length_all_xpath <- glue("
  parent::expr
    /parent::expr[
      expr[1]/SYMBOL_FUNCTION_CALL[text() = 'length']
      and (
        (expr[2]/expr[1]/SYMBOL_FUNCTION_CALL[text() = 'grep'] and expr[2]/expr[3] = {other_length_arg})
        or (
          expr[2]/expr[1]/SYMBOL_FUNCTION_CALL[text() = 'which']
          and {candidate_vec('expr[2]/expr[2]')} = {other_length_arg}
        )
      )
    ]
  ")
  known_logical_calls <- c(
    "grepl", "str_detect", "nzchar", "startsWith", "endsWith",
    "xor", "is.element", "duplicated",
    "is.na", "is.nan", "is.finite", "is.infinite",
    NULL
  )
  sum_inner_cond <- glue("
    expr[
      expr/SYMBOL_FUNCTION_CALL[{xp_text_in_table(known_logical_calls)}]
      or (EQ or NE or GT or LT or GE or LE or AND or OR or OP-EXCLAMATION)
      or SPECIAL[text() = '%in%' or text() = '%chin%']
    ]
  ")
  sum_any_xpath <- glue("
  parent::expr[
    {sum_inner_cond}
    and {any_parent_cond}
  ]")
  sum_all_xpath <- glue("
  parent::expr[
    {sum_inner_cond}
    and {candidate_vec('expr[2]')} = {other_length_arg}
  ]")

  Linter(linter_level = "expression", function(source_expression) {
    length_calls <- source_expression$xml_find_function_calls(c("which", "grep"))
    sum_calls <- source_expression$xml_find_function_calls("sum")
    any_expr <- combine_nodesets(
      xml_find_all_(length_calls, length_any_xpath),
      xml_find_all_(sum_calls, sum_any_xpath)
    )
    all_expr <- combine_nodesets(
      xml_find_all_(length_calls, length_all_xpath),
      xml_find_all_(sum_calls, sum_all_xpath)
    )

    # TODO(#2464): customize this?
    any_msg <- paste(
      "Use any() to express logical aggregations.",
      "For example, replace length(which(x == y)) == 0 with !any(x == y)."
    )
    all_msg <- paste(
      "Use all() to express logical aggregations.",
      "For example, replace length(which(x == y)) == length(x) with all(x == y)."
    )

    xml_nodes_to_lints(
      combine_nodesets(any_expr, all_expr),
      source_expression = source_expression,
      lint_message = c(rep(any_msg, length(any_expr)), rep(all_msg, length(all_expr))),
      type = "warning"
    )
  })
}
