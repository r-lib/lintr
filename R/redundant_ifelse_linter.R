#' Prevent ifelse() from being used to produce TRUE/FALSE or 1/0
#'
#' Expressions like `ifelse(x, TRUE, FALSE)` and `ifelse(x, FALSE, TRUE)` are
#'   redundant; just `x` or `!x` suffice in R code where logical vectors are a
#'   core data structure. `ifelse(x, 1, 0)` is also `as.numeric(x)`, but even
#'   this should only be needed rarely.
#'
#' @evalRd rd_tags("redundant_ifelse_linter")
#' @param allow10 Logical, default `FALSE`. If `TRUE`, usage like
#'   `ifelse(x, 1, 0)` is allowed, i.e., only usage like
#'   `ifelse(x, TRUE, FALSE)` is linted.
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
redundant_ifelse_linter <- function(allow10 = FALSE) {
  tf_xpath <- glue::glue("//expr[
    expr[1][SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]
    and expr[NUM_CONST[text() = 'TRUE']]
    and expr[NUM_CONST[text() = 'FALSE']]
  ]")

  num_xpath <- glue::glue("//expr[
    expr[1][SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]
    and expr[NUM_CONST[text() = '1' or text() = '1L']]
    and expr[NUM_CONST[text() = '0' or text() = '0L']]
  ]")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content
    lints <- list()

    tf_expr <- xml2::xml_find_all(xml, tf_xpath)
    matched_call <- xp_call_name(tf_expr)
    # [1] call; [2] logical condition
    first_arg <- xml2::xml_find_chr(tf_expr, "string(expr[3]/NUM_CONST)")
    second_arg <- xml2::xml_find_chr(tf_expr, "string(expr[4]/NUM_CONST)")
    tf_message <- sprintf(
      "Just use the logical condition (or its negation) directly instead of calling %s(x, %s, %s)",
      matched_call, first_arg, second_arg
    )
    lints <- c(lints, xml_nodes_to_lints(tf_expr, source_expression, tf_message, type = "warning"))

    if (!allow10) {
      num_expr <- xml2::xml_find_all(xml, num_xpath)
      matched_call <- xp_call_name(num_expr)
      # [1] call; [2] logical condiditon
      first_arg <- xml2::xml_find_chr(num_expr, "string(expr[3]/NUM_CONST)")
      second_arg <- xml2::xml_find_chr(num_expr, "string(expr[4]/NUM_CONST)")
      replacement <- ifelse(
        first_arg %in% c("0", "1") | second_arg %in% c("0", "1"),
        "as.numeric",
        "as.integer"
      )
      lint_message <- paste(
        sprintf("Prefer %s(x) to %s(x, %s, %s) if really needed,", replacement, matched_call, first_arg, second_arg),
        "but do note that R will usually convert logical vectors to 0/1 on the fly when needed."
      )
      lints <- c(lints, xml_nodes_to_lints(num_expr, source_expression, lint_message, type = "warning"))
    }

    return(lints)
  })
}
