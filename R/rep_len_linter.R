#' Require usage of rep_len(x, n) over rep(x, length.out = n)
#'
#' `rep(x, length.out = n)` calls `rep_len(x, n)` "under the hood". The latter
#'   is thus more direct and equally readable.
#'
#' @evalRd rd_tags("rep_len_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
rep_len_linter <- make_linter_from_xpath(
  # match starts-with(text(), 'len') to get common partial matching usage.
  # count(expr) is for cases using positional matching; see ?rep.
  xpath = "
  //SYMBOL_FUNCTION_CALL[text() = 'rep']
    /parent::expr
    /parent::expr[
      (
        SYMBOL_SUB[text() = 'length.out']
        or (not(SYMBOL_SUB) and count(expr) = 4)
      )
      and not(SYMBOL_SUB[text() = 'each'] or count(expr) = 5)
    ]
  ",
  lint_message = "Use rep_len(x, n) instead of rep(x, length.out = n)."
)
