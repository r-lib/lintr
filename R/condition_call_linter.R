#' Recommend usage of `call. = FALSE` in conditions
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

  if (!display_call) {
    # call. = TRUE can be expressed in two way:
    #  - either explicitly with call. = TRUE
    #  - or by implicitly relying on the default
    frag <- xp_or(call_xpath, no_call_xpath)
  } else {
    frag <- call_xpath
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

    if (display_call) {
      msg <- glue::glue(
        "Use {xp_call_name(bad_expr)}(.) to display call in error message"
      )
    } else {
      msg <- glue::glue(
        "Use {xp_call_name(bad_expr)}(., call. = FALSE)",
        " to not display call in error message"
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
