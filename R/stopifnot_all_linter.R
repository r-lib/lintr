#' Block usage of all() within stopifnot()
#'
#' `stopifnot(A)` actually checks `all(A)` "under the hood" if `A` is a vector,
#'   and produces a better error message than `stopifnot(all(A))` does.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "stopifnot(all(x > 0))",
#'   linters = stopifnot_all_linter()
#' )
#'
#' lint(
#'   text = "stopifnot(y > 3, all(x < 0))",
#'   linters = stopifnot_all_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "stopifnot(is.null(x) || all(x > 0))",
#'   linters = stopifnot_all_linter()
#' )
#'
#' lint(
#'   text = "assert_that(all(x > 0))",
#'   linters = stopifnot_all_linter()
#' )
#'
#' @evalRd rd_tags("stopifnot_all_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
stopifnot_all_linter <- make_linter_from_function_xpath(
  function_names = "stopifnot",
  xpath = "
  parent::expr
    /expr[expr/SYMBOL_FUNCTION_CALL[text() = 'all']]
  ",
  lint_message = paste(
    "Use stopifnot(x) instead of stopifnot(all(x)).",
    "stopifnot(x) runs all() 'under the hood' and provides a better error message in case of failure."
  )
)
