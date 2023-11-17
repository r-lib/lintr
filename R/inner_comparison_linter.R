#' Require == to be used outside of sapply() when comparing to a constant
#'
#' `sapply(x, function(xi) foo(xi) == 2)` is the same as `sapply(x, foo) == 2`,
#'   but misses the opportunity to vectorize the call to `==`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "sapply(x, function(xi) xi == 2)",
#'   linters = inner_comparison_linter()
#' )
#'
#' lint(
#'   text = "sapply(x, function(xi) sum(xi) > 0)",
#'   linters = inner_comparison_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "sapply(x, sum) > 0",
#'   linters = inner_comparison_linter()
#' )
#'
#' @evalRd rd_tags("inner_comparison_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
inner_comparison_linter <- make_linter_from_xpath(
  # OP-PLUS: condition for complex literal, e.g. 0+2i.
  # NB: this includes 0+3 and TRUE+FALSE, which are also fine.
  xpath = "
  //SYMBOL_FUNCTION_CALL[text() = 'sapply' or text() = 'vapply']
    /parent::expr
    /parent::expr
    /expr[FUNCTION]
    /expr[
      (EQ or NE or GT or GE or LT or LE)
      and expr[
        NUM_CONST
        or STR_CONST
        or (OP-PLUS and count(expr/NUM_CONST) = 2)
      ]
    ]
  ",
  lint_message = paste(
    "Compare to a constant after calling sapply()/vapply()",
    "to get the full benefits of vectorization.",
    "Prefer sapply(x, foo) == 2 over sapply(x, function(xi) foo(xi) == 2)."
  )
)
