#' Require usage of nlevels over length(levels(.))
#'
#' `length(levels(x))` is the same as `nlevels(x)`, but harder to read.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "length(levels(x))",
#'   linters = length_levels_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "length(c(levels(x), levels(y)))",
#'   linters = length_levels_linter()
#' )
#'
#' @evalRd rd_tags("length_levels_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
length_levels_linter <- make_linter_from_function_xpath(
  function_names = "levels",
  xpath = "
  parent::expr
    /parent::expr[expr/SYMBOL_FUNCTION_CALL[text() = 'length']]
  ",
  lint_message = "nlevels(x) is better than length(levels(x))."
)
