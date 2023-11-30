#' Recommend usage of `call. = FALSE` in conditions
#'
#' This linter, with the default `display_call = FALSE`, enforces the
#' recommendation of the tidyverse design guide regarding displaying error
#' calls: <https://design.tidyverse.org/err-call.html>
#'
#' @param display_call Logical specifying expected behaviour regarding `call.`
#' argument in conditions.
#'   - `NA` forces providing `call.=` but ignores its value (this can be used in
#'     cases where you expect a mix of `call. = FALSE` and `call. = TRUE`)
#'   - lints `call. = FALSE`
#'   - forces `call. = FALSE` (lints `call. = TRUE` or missing `call.=` value)
#'
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "stop('test')",
#'   linters = condition_call_linter()
#' )
#'
#' lint(
#'   text = "stop('test', call. = TRUE)",
#'   linters = condition_call_linter()
#' )
#'
#' lint(
#'   text = "stop('test', call. = FALSE)",
#'   linters = condition_call_linter(display_call = TRUE)
#' )
#'
#' lint(
#'   text = "stop('this is a', 'test', call. = FALSE)",
#'   linters = condition_call_linter(display_call = TRUE)
#' )
#'
#' # okay
#' lint(
#'   text = "stop('test', call. = FALSE)",
#'   linters = condition_call_linter()
#' )
#'
#' lint(
#'   text = "stop('this is a', 'test', call. = FALSE)",
#'   linters = condition_call_linter()
#' )
#'
#' lint(
#'   text = "stop('test', call. = TRUE)",
#'   linters = condition_call_linter(display_call = TRUE)
#' )
#'
#' @evalRd rd_tags("condition_call_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
condition_call_linter <- function(display_call = FALSE) {
  call_xpath <- glue::glue("
    following-sibling::SYMBOL_SUB[text() = 'call.']
    /following-sibling::expr[1]
    /NUM_CONST[text() = '{!display_call}']
  ")
  no_call_xpath <- "
    parent::expr[
      count(SYMBOL_SUB[text() = 'call.']) = 0
    ]
  "

  if (is.na(display_call)) {
    frag <- no_call_xpath
  } else if (display_call) {
    frag <- call_xpath
  } else {
    # call. = TRUE can be expressed in two way:
    #  - either explicitly with call. = TRUE
    #  - or by implicitly relying on the default
    frag <- xp_or(call_xpath, no_call_xpath)
  }

  xpath <- glue::glue("
    //SYMBOL_FUNCTION_CALL[text() = 'stop' or text() = 'warning']
    /parent::expr[{frag}]
    /parent::expr
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    if (is.na(display_call)) {
      msg <- glue::glue(
        "Provide an explicit value for call. in {xp_call_name(bad_expr)}()."
      )
    } else if (display_call) {
      msg <- glue::glue(
        "Use {xp_call_name(bad_expr)}(.) to display call in error message."
      )
    } else {
      msg <- glue::glue(
        "Use {xp_call_name(bad_expr)}(., call. = FALSE)",
        " to not display call in error message."
      )
    }

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = msg,
      type = "warning"
    )
  })
}
