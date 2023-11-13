#' Block usage of all() within stopifnot()
#'
#' `stopifnot(A)` actually checks `all(A)` "under the hood" if `A` is a vector,
#'   and produces a better error message than `stopifnot(all(A))` does.
#'
#' @evalRd rd_tags("stopifnot_all_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
stopifnot_all_linter <- make_linter_from_xpath(
  xpath = "
  //SYMBOL_FUNCTION_CALL[text() = 'stopifnot']
    /parent::expr
    /parent::expr
    /expr[expr/SYMBOL_FUNCTION_CALL[text() = 'all']]
  ",
  lint_message = paste(
    "Calling stopifnot(all(x)) is redundant. stopifnot(x) runs all()",
    "'under the hood' and provides a better error message in case of failure."
  )
)
