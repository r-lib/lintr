#' Duplicate argument linter
#'
#' Check for duplicate arguments in function calls. Some cases are run-time errors
#' (e.g. `mean(x = 1:5, x = 2:3)`), otherwise this linter is used to discourage
#'  explicitly providing duplicate names to objects (e.g. `c(a = 1, a = 2)`).
#'  Duplicate-named objects are hard to work with programmatically and
#'  should typically be avoided.
#'
#' @param except A character vector of function names as exceptions. Defaults to
#'   functions that allow sequential updates to variables, currently `dplyr::mutate()`
#'   and `dplyr::transmute()`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "list(x = 1, x = 2)",
#'   linters = duplicate_argument_linter()
#' )
#'
#' lint(
#'   text = "fun(arg = 1, arg = 2)",
#'   linters = duplicate_argument_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "list(x = 1, x = 2)",
#'   linters = duplicate_argument_linter(except = "list")
#' )
#'
#' lint(
#'   text = "df %>% dplyr::mutate(x = a + b, x = x + d)",
#'   linters = duplicate_argument_linter()
#' )
#'
#' @evalRd rd_tags("duplicate_argument_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
duplicate_argument_linter <- function(except = c("mutate", "transmute")) {
  # NB: approach checking for duplicates in XPath is hard because of
  #   quoted names, e.g. foo(a = 1, `a` = 2), so compute duplicates in R
  xpath_call_with_args <- glue("
    //EQ_SUB[not(
      preceding-sibling::expr/SYMBOL_FUNCTION_CALL[{ xp_text_in_table(except) }]
    )]
      /parent::expr
  ")
  xpath_arg_name <- "./EQ_SUB/preceding-sibling::*[1]"

  Linter(linter_level = "file", function(source_expression) {
    xml <- source_expression$full_xml_parsed_content
    if (is.null(xml)) return(list())

    calls <- xml_find_all(xml, xpath_call_with_args)

    all_arg_nodes <- lapply(calls, xml_find_all, xpath_arg_name)
    arg_names <- lapply(all_arg_nodes, get_r_string)
    is_duplicated <- lapply(arg_names, duplicated)

    xml_nodes_to_lints(
      unlist(all_arg_nodes, recursive = FALSE)[unlist(is_duplicated)],
      source_expression = source_expression,
      lint_message = "Avoid duplicate arguments in function calls.",
      type = "warning"
    )
  })
}
