#' Parenthesis before body linter
#'
#' Check that there is a space between right parenthesis and a body expression.
#'
#' @evalRd rd_tags("paren_body_linter")
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "function(x)x + 1",
#'   linters = paren_body_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "function(x) x + 1",
#'   linters = paren_body_linter()
#' )
#'
#' @seealso
#' - [linters] for a complete list of linters available in lintr.
#' - <https://style.tidyverse.org/syntax.html#parentheses>
#' @export
paren_body_linter <- make_linter_from_xpath(
  # careful to do recursive search to the less common OP-RIGHT-PAREN
  #   and forcond nodes (vs. //expr) for performance -- there can
  #   be O(100K) <expr> nodes but in all but pathological examples,
  #   these other nodes will only be a small fraction of this amount.
  # note also that <forcond> only has one following-sibling::expr.
  xpath = "
  //OP-RIGHT-PAREN[
    @end = following-sibling::expr[1]/@start - 1
    and @line1 = following-sibling::expr[1]/@line1
    and (
      preceding-sibling::FUNCTION
      or preceding-sibling::OP-LAMBDA
      or preceding-sibling::IF
      or preceding-sibling::WHILE
    )
  ]
    /following-sibling::expr[1]
  |
  //forcond[
    @line1 = following-sibling::expr/@line2
    and OP-RIGHT-PAREN/@col1 = following-sibling::expr/@col1 - 1
  ]
    /following-sibling::expr
  ",
  lint_message = "Put a space between a right parenthesis and a body expression.",
  type = "style"
)
