#' Require usage of deparse1() over paste(deparse(x), collapse = ...)
#'
#' `paste(deparse(x), collapse = ...)` is the same as `deparse1(x)`, but
#'   harder to read.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = 'paste(deparse(x), collapse = " ")',
#'   linters = deparse1_linter()
#' )
#'
#' lint(
#'   text = 'paste0(deparse(x), collapse = "")',
#'   linters = deparse1_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "deparse1(x)",
#'   linters = deparse1_linter()
#' )
#'
#' lint(
#'   text = "deparse(x)",
#'   linters = deparse1_linter()
#' )
#'
#' @evalRd rd_tags("deparse1_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
deparse1_linter <- make_linter_from_function_xpath(
  function_names = "deparse",
  xpath = "
  parent::expr
    /parent::expr[
      expr[1]/SYMBOL_FUNCTION_CALL[text() = 'paste' or text() = 'paste0']
      and SYMBOL_SUB[text() = 'collapse']
      and count(expr) = 3
    ]
  ",
  lint_message = "Use deparse1(x) instead of paste(deparse(x), collapse = ...)."
)
