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
  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    tf_xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]
      and expr[NUM_CONST[text() = 'TRUE']]
      and expr[NUM_CONST[text() = 'FALSE']]
    ]")
    tf_expr <- xml2::xml_find_all(xml, tf_xpath)
    tf_lints <- xml_nodes_to_lints(
      tf_expr,
      source_expression = source_expression,
      lint_message = function(expr) {
        matched_call <- xp_call_name(expr)
        # [1] call; [2] logical condiditon
        first_arg <- xml2::xml_find_chr(expr, "string(expr[3]/NUM_CONST)")
        second_arg <- xml2::xml_find_chr(expr, "string(expr[4]/NUM_CONST)")
        sprintf(
          "Just use the logical condition (or its negation) directly instead of calling %s(x, %s, %s)",
          matched_call, first_arg, second_arg
        )
      },
      type = "warning"
    )

    if (allow10) {
      num_lints <- NULL
    } else {
      num_xpath <- glue::glue("//expr[
        expr[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]
        and expr[NUM_CONST[text() = '1' or text() = '1L']]
        and expr[NUM_CONST[text() = '0' or text() = '0L']]
      ]")
      num_expr <- xml2::xml_find_all(xml, num_xpath)
      num_lints <- xml_nodes_to_lints(
        num_expr,
        source_expression = source_expression,
        lint_message = function(expr) {
          matched_call <- xp_call_name(expr)
          # [1] call; [2] logical condiditon
          first_arg <- xml2::xml_find_chr(expr, "string(expr[3]/NUM_CONST)")
          second_arg <- xml2::xml_find_chr(expr, "string(expr[4]/NUM_CONST)")
          replacement <- if (any(c(first_arg, second_arg) %in% c("0", "1"))) "as.numeric" else "as.integer"
          message <- sprintf(
            "Prefer %s(x) to %s(x, %s, %s) if really needed,",
            replacement, matched_call, first_arg, second_arg
          )
          paste(message, "but do note that R will usually convert logical vectors to 0/1 on the fly when needed.")
        },
        type = "warning"
      )
    }

    return(c(tf_lints, num_lints))
  })
}
