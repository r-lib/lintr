#' Chained equality check linter
#'
#' Report the use of chained equality checks where `%in%` would be more
#' appropriate
#'
#' @examples
#' # lints
#' lint(
#'   text = "x == 'a' | x == 'b'",
#'   linters = in_linter()
#' )
#'
#' # This only makes sense in the case x if of length 1
#' lint(
#'   text = "x == 'a' || x == 'b'",
#'   linters = in_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x %in% c('a', 'b')",
#'   linters = in_linter()
#' )
#'
#' @evalRd rd_tags("in_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
in_linter <- make_linter_from_xpath(
  xpath = "(//OR|//OR2)[
    preceding-sibling::expr[EQ]
    and following-sibling::expr[EQ]
    and preceding-sibling::expr/expr/SYMBOL/text() = following-sibling::expr/expr/SYMBOL/text()
  ]",
  lint_message = "Use %in% to test equality of a variable against multiple values."
)
