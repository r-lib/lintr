#' Require usage of grep over which(grepl(.))
#'
#' `which(grepl(pattern, x))` is the same as `grep(pattern, x)`, but harder
#'   to read and requires two passes over the vector.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "which(grepl('^a', x))",
#'   linters = which_grepl_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "which(grepl('^a', x) | grepl('^b', x))",
#'   linters = which_grepl_linter()
#' )
#'
#' @evalRd rd_tags("which_grepl_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
which_grepl_linter <- make_linter_from_function_xpath(
  function_names = "grepl",
  xpath = "
  parent::expr
    /parent::expr[expr/SYMBOL_FUNCTION_CALL[text() = 'which']]
  ",
  lint_message = "grep(pattern, x) is better than which(grepl(pattern, x))."
)
