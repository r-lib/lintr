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
  comparator_nodes <- infix_metadata$xml_tag[infix_metadata$comparator]
  logical_nodes <- c(comparator_nodes, "AND", "OR", "OP-EXCLAMATION")

  zero_expr <- "(EQ or NE or GT or LE) and expr/NUM_CONST[text() = '0' or text() = '0L']"
  one_expr <- "(LT or GE) and expr/NUM_CONST[text() = '1' or text() = '1L']"
  any_parent_cond <- glue("parent::expr[({zero_expr}) or ({one_expr})]")
  length_cond <- "expr[1]/SYMBOL_FUNCTION_CALL[text() = 'length']"

  known_logical_calls <- c(
    "grepl", "str_detect", "nzchar", "startsWith", "endsWith",
    "xor", "is.element", "duplicated",
    "is.na", "is.nan", "is.finite", "is.infinite",
    NULL
  )
  first_arg_logical_calls <- setdiff(known_logical_calls, "grepl")
  second_arg_logical_calls <- c("grepl", "xor")

  logical_special_node <- "SPECIAL[text() = '%in%' or text() = '%chin%']"

  # 'x' in the paired length(x) call, e.g. in sum(x == y) == length(x)
  paired_length_arg <- glue("
    (
      following-sibling::expr[1][parent::expr[EQ or NE or LT or GE]]
      | preceding-sibling::expr[1][parent::expr[EQ or NE or GT or LE]]
    )
    [{length_cond}]/expr[2]
  ")

  # Step from a logical operation/call to its vector argument, e.g. x == y -> x
  logical_operand_xpath <- glue("expr[
    parent::expr[{xp_or(logical_nodes)} or *[1][self::OP-LEFT-PAREN]]
    or following-sibling::{logical_special_node}
    or preceding-sibling::expr[1]/SYMBOL_FUNCTION_CALL[{xp_text_in_table(first_arg_logical_calls)}]
    or preceding-sibling::expr[2]/SYMBOL_FUNCTION_CALL[{xp_text_in_table(second_arg_logical_calls)}]
  ]")

  # Unwrap up to 3 nested logical ops to reach the underlying vector(s) 'x'
  logical_operands <- function(base) {
    glue("(
      {base}
      | {base}/{logical_operand_xpath}
      | {base}/{logical_operand_xpath}/{logical_operand_xpath}
      | {base}/{logical_operand_xpath}/{logical_operand_xpath}/{logical_operand_xpath}
    )")
  }

  length_any_xpath <- glue("
  parent::expr
    /parent::expr[
      expr/SYMBOL_FUNCTION_CALL[text() = 'length']
      and {any_parent_cond}
    ]
  ")
  length_comparison_xpath <- glue("
  parent::expr
    /parent::expr[{length_cond}]
    /parent::expr[
      ({xp_or(comparator_nodes)})
      and count(expr[{length_cond}]) = 2
    ]
  ")
  length_all_xpath <- glue("
  expr[
    (
      expr[2]/expr[1]/SYMBOL_FUNCTION_CALL[text() = 'grep']
      and expr[2]/expr[3] = {paired_length_arg})
    or (
      expr[2]/expr[1]/SYMBOL_FUNCTION_CALL[text() = 'which']
      and {logical_operands('expr[2]/expr[2]')} = {paired_length_arg}
    )
  ]
  ")
  sum_inner_cond <- glue("
    expr/SYMBOL_FUNCTION_CALL[{xp_text_in_table(known_logical_calls)}]
    or {xp_or(logical_nodes)}
    or {logical_special_node}
  ")
  sum_any_xpath <- glue("
  parent::expr[
    expr[{sum_inner_cond}]
    and {any_parent_cond}
  ]")
  sum_comparison_xpath <- glue("
  parent::expr[
    count(expr) = 2 + count(SYMBOL_SUB[text() = 'na.rm'])
    and expr[2][{sum_inner_cond}]
  ]
    /parent::expr[
      ({xp_or(comparator_nodes)})
      and expr/{length_cond}
    ]
  ")
  sum_all_xpath <- glue("
  expr[
    expr[1]/SYMBOL_FUNCTION_CALL[text() = 'sum']
    and {logical_operands('expr[2]')} = {paired_length_arg}
  ]")

  Linter(linter_level = "expression", function(source_expression) {
    length_calls <- source_expression$xml_find_function_calls(c("which", "grep"))
    sum_calls <- source_expression$xml_find_function_calls("sum")

    length_any_expr <- xml_find_all_(length_calls, length_any_xpath)
    sum_any_expr <- xml_find_all_(sum_calls, sum_any_xpath)

    length_all_expr <- length_calls |>
      xml_find_all_(length_comparison_xpath) |>
      strip_comments_from_subtree() |>
      xml_find_all_(length_all_xpath)
    sum_all_expr <- sum_calls |>
      xml_find_all_(sum_comparison_xpath) |>
      strip_comments_from_subtree() |>
      xml_find_all_(sum_all_xpath)

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
      combine_nodesets(length_any_expr, sum_any_expr, length_all_expr, sum_all_expr),
      source_expression = source_expression,
      lint_message = rep(
        c(any_msg, all_msg),
        c(
          length(length_any_expr) + length(sum_any_expr),
          length(length_all_expr) + length(sum_all_expr)
        )
      ),
      type = "warning"
    )
  })
}
