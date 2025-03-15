#' Recommend direct usage of `data.frame()` to create a data.frame from a list
#'
#' It is possible to create a data.frame from a list of columns with `data.frame()`
#' or `list2DF()` (since \R 4.0.0 and if recycling is not required), rather than
#' iteratively adding columns with `cbind()`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "do.call(cbind.data.frame, x)",
#'   linters = list2df_linter()
#' )
#'
#' lint(
#'   text = "do.call(cbind.data.frame, list(a = 1, b = 1:10))",
#'   linters = list2df_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "list2df(x)",
#'   linters = list2df_linter()
#' )
#'
#' lint(
#'   text = "data.frame(list(a = 1, b = 1:10))",
#'   linters = list2df_linter()
#' )
#'
#' @evalRd rd_tags("list2df_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
list2df_linter <- make_linter_from_function_xpath(
  function_names = "do.call",
  xpath = "./following-sibling::expr[1]/SYMBOL[text() = 'cbind.data.frame']",
  lint_message = paste0(
    "Instead of `do.call(cbind.data.frame, lst)`, use `data.frame(lst)`, ",
    "or `list2DF(lst)` if available and recyclying is not required"
  ),
  type = "warning",
  level = "expression"
)
