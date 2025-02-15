#' Block usage of for loops directly overwriting the indexing variable
#'
#' `for (x in x)` is a poor choice of indexing variable. This overwrites
#'   `x` in the calling scope and is confusing to read.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "for (x in x) { TRUE }",
#'   linters = for_loop_index_linter()
#' )
#'
#' lint(
#'   text = "for (x in foo(x, y)) { TRUE }",
#'   linters = for_loop_index_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "for (xi in x) { TRUE }",
#'   linters = for_loop_index_linter()
#' )
#'
#' lint(
#'   text = "for (col in DF$col) { TRUE }",
#'   linters = for_loop_index_linter()
#' )
#'
#' @evalRd rd_tags("for_loop_index_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
for_loop_index_linter <- make_linter_from_xpath(
  xpath = "
  //forcond
    /SYMBOL[text() =
      following-sibling::expr
        //SYMBOL[not(parent::expr[OP-DOLLAR or OP-AT or preceding-sibling::OP-LEFT-BRACKET])]
        /text()
    ]
  ",
  lint_message = "Don't re-use any sequence symbols as the index symbol in a for loop."
)
