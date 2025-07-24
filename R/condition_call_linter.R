#' Recommend usage of `call. = FALSE` in conditions
#'
#' This linter, with the default `display_call = FALSE`, enforces the
#' recommendation of the tidyverse design guide regarding displaying error
#' calls.
#'
#' @param display_call Logical specifying expected behavior regarding `call.`
#' argument in conditions.
#'   - `NA` forces providing `call. =` but ignores its value (this can be used in
#'     cases where you expect a mix of `call. = FALSE` and `call. = TRUE`)
#'   - `TRUE` lints `call. = FALSE`
#'   - `FALSE` forces `call. = FALSE` (lints `call. = TRUE` or missing `call. =` value)
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
#' @seealso
#'   - [linters] for a complete list of linters available in lintr.
#'   - <https://design.tidyverse.org/err-call.html>>
#' @export
condition_call_linter <- function(display_call = FALSE) {
  call_xpath <- glue::glue("
    following-sibling::expr/STR_CONST
    and following-sibling::SYMBOL_SUB[text() = 'call.']
      /following-sibling::expr[1]
      /NUM_CONST[text() = '{!display_call}']
  ")
  no_call_xpath <- "
    following-sibling::expr/STR_CONST
    and parent::expr[not(SYMBOL_SUB[text() = 'call.'])]
  "

  if (is.na(display_call)) {
    call_cond <- no_call_xpath
    msg_fmt <- "Provide an explicit value for `call.` in %s()."
  } else if (display_call) {
    call_cond <- call_xpath
    msg_fmt <- "Use %s(.) to display the call in an error message."
  } else {
    # call. = TRUE can be expressed in two way:
    #  - either explicitly with call. = TRUE
    #  - or by implicitly relying on the default
    call_cond <- xp_or(call_xpath, no_call_xpath)
    msg_fmt <- "Use %s(., call. = FALSE) not to display the call in an error message."
  }

  xpath <- glue::glue("./self::*[{call_cond}]/parent::expr")

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls(c("stop", "warning"))

    bad_expr <- xml_find_all(xml_calls, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = sprintf(msg_fmt, xp_call_name(bad_expr)),
      type = "warning"
    )
  })
}
