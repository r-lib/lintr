#' Require usage of grep over which(grepl(.))
#'
#' `which(grepl(pattern, x))` is the same as `grep(pattern, x)`, but harder
#'   to read and requires two passes over the vector.
#'
#' @evalRd rd_tags("which_grepl_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
which_grepl_linter <- make_linter_from_xpath(
  xpath = "
  //SYMBOL_FUNCTION_CALL[text() = 'grepl']
    /parent::expr
    /parent::expr
    /parent::expr[expr/SYMBOL_FUNCTION_CALL[text() = 'which']]
  ",
  lint_message = "grep(pattern, x) is better than which(grepl(pattern, x))."
)
