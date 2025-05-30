#' Recommend direct usage of `data.frame()` to create a data.frame from a list
#'
#' [list2DF()] is the preferred way to turn a list of columns into a data.frame.
#'   Note that it doesn't support recycling; if that's required, use [data.frame()].
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "do.call(cbind.data.frame, x)",
#'   linters = list2df_linter()
#' )
#'
#' lint(
#'   text = "do.call('cbind.data.frame', x)",
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
list2df_linter <- function() {

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls("do.call")

    xml_calls_nolambda <- xml_find_all(
      xml_calls,
      "./following-sibling::expr[1][SYMBOL or STR_CONST]"
    )

    bad_expr <- xml_calls_nolambda[
      get_r_string(xml_calls_nolambda) == "cbind.data.frame"
    ]

    xml_nodes_to_lints(
      xml2::xml_parent(bad_expr),
      source_expression = source_expression,
      lint_message = paste(
        "Use `list2DF(lst)` instead of `do.call(cbind.data.frame, lst)`.",
        "If recycling is required, use `data.frame(lst)`."
      ),
      type = "warning"
    )
  })
}
