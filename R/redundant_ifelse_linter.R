#' Prevent ifelse() from being used to produce TRUE/FALSE or 1/0
#'
#' Expressions like `ifelse(x, TRUE, FALSE)` and `ifelse(x, FALSE, TRUE)` are
#'   redundant; just `x` or `!x` suffice in R code where logical vectors are a
#'   core data structure. `ifelse(x, 1, 0)` is also `as.numeric(x)`, but even
#'   this should only be needed rarely.
#'
#' @evalRd rd_tags("redundant_ifelse_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
redundant_ifelse_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$xml_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    xpath <- glue::glue("//expr[
      expr[SYMBOL_FUNCTION_CALL[ {xp_text_in_table(ifelse_funs)} ]]
      and (
        (expr[NUM_CONST[text() = 'TRUE']] and expr[NUM_CONST[text() = 'FALSE']])
        or (
          expr[NUM_CONST[text() = '1' or text() = '1L']]
          and expr[NUM_CONST[text() = '0' or text() = '0L']]
        )
      )
    ]")
    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = function(expr) {
        matched_call <- xml2::xml_text(xml2::xml_find_first(expr, "expr/SYMBOL_FUNCTION_CALL"))
        # [1] call; [2] logical condiditon
        first_arg <- xml2::xml_text(xml2::xml_find_first(expr, "expr[3]/NUM_CONST"))
        second_arg <- xml2::xml_text(xml2::xml_find_first(expr, "expr[4]/NUM_CONST"))
        if (first_arg %in% c("TRUE", "FALSE")) {
          message <- sprintf(
            "Just use the logical condition (or its negation) directly instead of calling %s(x, %s, %s)",
            matched_call, first_arg, second_arg
          )
        } else {
          if (any(c(first_arg, second_arg) %in% c("0", "1"))) {
            replacement <- "as.numeric"
          } else {
            replacement <- "as.integer"
          }
          message <- sprintf(
            "Prefer %s(x) to %s(x, %s, %s) if really needed,",
            replacement, matched_call, first_arg, second_arg
          )
          paste(message, "but do note that R will usually convert logical vectors to 0/1 on the fly  when needed.")
        }
        message
      },
      type = "warning"
    ))
  })
}
