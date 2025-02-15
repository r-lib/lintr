#' Prevent `ifelse()` from being used to produce `TRUE`/`FALSE` or `1`/`0`
#'
#' Expressions like `ifelse(x, TRUE, FALSE)` and `ifelse(x, FALSE, TRUE)` are
#'   redundant; just `x` or `!x` suffice in R code where logical vectors are a
#'   core data structure. `ifelse(x, 1, 0)` is also `as.numeric(x)`, but even
#'   this should be needed only rarely.
#'
#' @evalRd rd_tags("redundant_ifelse_linter")
#' @param allow10 Logical, default `FALSE`. If `TRUE`, usage like
#'   `ifelse(x, 1, 0)` is allowed, i.e., only usage like
#'   `ifelse(x, TRUE, FALSE)` is linted.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "ifelse(x >= 2.5, TRUE, FALSE)",
#'   linters = redundant_ifelse_linter()
#' )
#'
#' lint(
#'   text = "ifelse(x < 2.5, 1L, 0L)",
#'   linters = redundant_ifelse_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x >= 2.5",
#'   linters = redundant_ifelse_linter()
#' )
#'
#' # Note that this is just to show the strict equivalent of the example above;
#' # converting to integer is often unnecessary and the logical vector itself
#' # should suffice.
#' lint(
#'   text = "as.integer(x < 2.5)",
#'   linters = redundant_ifelse_linter()
#' )
#'
#' lint(
#'   text = "ifelse(x < 2.5, 1L, 0L)",
#'   linters = redundant_ifelse_linter(allow10 = TRUE)
#' )
#'
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
redundant_ifelse_linter <- function(allow10 = FALSE) {
  tf_xpath <- glue("
  parent::expr[
    expr[position() <= 4 and NUM_CONST[text() = 'TRUE']]
    and expr[position() <= 4 and NUM_CONST[text() = 'FALSE']]
    and (
      count(expr) = 4
      or expr[5]/NUM_CONST[text() = 'NA']
    )
  ]")

  num_xpath <- glue("
  parent::expr[
    expr[position() <= 4 and NUM_CONST[text() = '1' or text() = '1L']]
    and expr[position() <= 4 and NUM_CONST[text() = '0' or text() = '0L']]
    and (
      count(expr) = 4
      or expr[5]/NUM_CONST[text() = 'NA' or text() = 'NA_integer_' or text() = 'NA_real_']
    )
  ]")

  Linter(linter_level = "expression", function(source_expression) {
    xml_targets <- source_expression$xml_find_function_calls(ifelse_funs)

    lints <- list()

    tf_expr <- xml_find_all(xml_targets, tf_xpath)
    matched_call <- xp_call_name(tf_expr)
    # [1] call; [2] logical condition
    first_arg <- xml_find_chr(tf_expr, "string(expr[3]/NUM_CONST)")
    second_arg <- xml_find_chr(tf_expr, "string(expr[4]/NUM_CONST)")
    tf_message <- sprintf(
      "Just use the logical condition (or its negation) directly instead of calling %s(x, %s, %s)",
      matched_call, first_arg, second_arg
    )
    lints <- c(lints, xml_nodes_to_lints(tf_expr, source_expression, tf_message, type = "warning"))

    if (!allow10) {
      num_expr <- xml_find_all(xml_targets, num_xpath)
      matched_call <- xp_call_name(num_expr)
      # [1] call; [2] logical condition
      first_arg <- xml_find_chr(num_expr, "string(expr[3]/NUM_CONST)")
      second_arg <- xml_find_chr(num_expr, "string(expr[4]/NUM_CONST)")
      is_numeric_01 <- first_arg %in% c("0", "1") | second_arg %in% c("0", "1")
      coercion_function <- ifelse(is_numeric_01, "as.numeric", "as.integer")
      is_negated <- first_arg %in% c("0", "0L")
      replacement_argument <- ifelse(is_negated, "!x", "x")
      lint_message <- paste(
        sprintf(
          "Prefer %s(%s) to %s(x, %s, %s) if really needed.",
          coercion_function, replacement_argument, matched_call, first_arg, second_arg
        )
      )
      lints <- c(lints, xml_nodes_to_lints(num_expr, source_expression, lint_message, type = "warning"))
    }

    lints
  })
}
