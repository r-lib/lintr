#' Undesirable function linter
#'
#' Report the use of undesirable functions and suggest an alternative.
#'
#' @param fun Named character vector. `names(fun)` correspond to undesirable functions,
#'   while the values give a description of why the function is undesirable.
#'   If `NA`, no additional information is given in the lint message. Defaults to
#'   [default_undesirable_functions]. To make small customizations to this list,
#'   use [modify_defaults()].
#' @param symbol_is_undesirable Whether to consider the use of an undesirable function
#'   name as a symbol undesirable or not.
#'
#' @examples
#' # defaults for which functions are considered undesirable
#' names(default_undesirable_functions)
#'
#' # will produce lints
#' lint(
#'   text = "sapply(x, mean)",
#'   linters = undesirable_function_linter()
#' )
#'
#' lint(
#'   text = "log10(x)",
#'   linters = undesirable_function_linter(fun = c("log10" = NA))
#' )
#'
#' lint(
#'   text = "log10(x)",
#'   linters = undesirable_function_linter(fun = c("log10" = "use log()"))
#' )
#'
#' lint(
#'   text = 'dir <- "path/to/a/directory"',
#'   linters = undesirable_function_linter(fun = c("dir" = NA))
#' )
#'
#' # okay
#' lint(
#'   text = "vapply(x, mean, FUN.VALUE = numeric(1))",
#'   linters = undesirable_function_linter()
#' )
#'
#' lint(
#'   text = "log(x, base = 10)",
#'   linters = undesirable_function_linter(fun = c("log10" = "use log()"))
#' )
#'
#' lint(
#'   text = 'dir <- "path/to/a/directory"',
#'   linters = undesirable_function_linter(fun = c("dir" = NA), symbol_is_undesirable = FALSE)
#' )
#'
#' @evalRd rd_tags("undesirable_function_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
undesirable_function_linter <- function(fun = default_undesirable_functions,
                                        symbol_is_undesirable = TRUE) {
  stopifnot(is.logical(symbol_is_undesirable))
  if (is.null(names(fun)) || !all(nzchar(names(fun))) || length(fun) == 0L) {
    cli_abort(c(
      x = "{.arg fun} should be a non-empty named character vector.",
      i = "Use missing elements to indicate default messages."
    ))
  }

  xp_condition <- xp_and(
    paste0(
      "not(parent::expr/preceding-sibling::expr[last()][SYMBOL_FUNCTION_CALL[",
      xp_text_in_table(c("library", "require")),
      "]])"
    ),
    "not(parent::expr[OP-DOLLAR or OP-AT])"
  )

  if (symbol_is_undesirable) {
    symbol_xpath <- glue("//SYMBOL[({xp_text_in_table(names(fun))}) and {xp_condition}]")
  }
  xpath <- glue("SYMBOL_FUNCTION_CALL[{xp_condition}]")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content
    xml_calls <- source_expression$xml_find_function_calls(names(fun))

    matched_nodes <- xml_find_all(xml_calls, xpath)
    if (symbol_is_undesirable) {
      matched_nodes <- combine_nodesets(matched_nodes, xml_find_all(xml, symbol_xpath))
    }

    fun_names <- get_r_string(matched_nodes)

    msgs <- vapply(
      stats::setNames(nm = unique(fun_names)),
      function(fun_name) {
        msg <- sprintf('Avoid undesirable function "%s".', fun_name)
        alternative <- fun[[fun_name]]
        if (!is.na(alternative)) {
          msg <- paste(msg, sprintf("As an alternative, %s.", alternative))
        }
        msg
      },
      character(1L)
    )

    xml_nodes_to_lints(
      matched_nodes,
      source_expression = source_expression,
      lint_message = unname(msgs[fun_names])
    )
  })
}
