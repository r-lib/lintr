#' Require usage of `lengths()` where possible
#'
#' [lengths()] is a function that was added to base R in version 3.2.0 to
#'   get the length of each element of a list. It is equivalent to
#'   `sapply(x, length)`, but faster and more readable.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "sapply(x, length)",
#'   linters = lengths_linter()
#' )
#'
#' lint(
#'   text = "vapply(x, length, integer(1L))",
#'   linters = lengths_linter()
#' )
#'
#' lint(
#'   text = "purrr::map_int(x, length)",
#'   linters = lengths_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "lengths(x)",
#'   linters = lengths_linter()
#' )
#'
#' @evalRd rd_tags("lengths_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
lengths_linter <- make_linter_from_function_xpath(
  function_names = c("sapply", "vapply", "map_int", "map_dbl"),
  xpath = "parent::expr[expr/SYMBOL[text() = 'length']]",
  lint_message = "Use lengths() to find the length of each element in a list."
)
