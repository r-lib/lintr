#' Require usage of rep_len(x, n) over rep(x, length.out = n)
#'
#' `rep(x, length.out = n)` calls `rep_len(x, n)` "under the hood". The latter
#'   is thus more direct and equally readable.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "rep(1:3, length.out = 10)",
#'   linters = rep_len_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "rep_len(1:3, 10)",
#'   linters = rep_len_linter()
#' )
#'
#' lint(
#'   text = "rep(1:3, each = 2L, length.out = 10L)",
#'   linters = rep_len_linter()
#' )
#'
#' @evalRd rd_tags("rep_len_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
rep_len_linter <- make_linter_from_function_xpath(
  function_names = "rep",
  # count(expr) is for cases using positional matching; see ?rep.
  xpath = "
    parent::expr[
      (
        SYMBOL_SUB[text() = 'length.out']
        or (not(SYMBOL_SUB) and count(expr) = 4)
      )
      and not(SYMBOL_SUB[text() = 'each'] or count(expr) = 5)
    ]
  ",
  lint_message = "Use rep_len(x, n) instead of rep(x, length.out = n)."
)
