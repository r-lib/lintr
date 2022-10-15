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
for_loop_index_linter <- function() {
  xpath <- "
  //forcond
    /SYMBOL[text() =
      following-sibling::expr
        //SYMBOL[not(
          preceding-sibling::OP-DOLLAR
          or parent::expr[preceding-sibling::OP-LEFT-BRACKET]
        )]
        /text()
    ]
  "

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Don't re-use any sequence symbols as the index symbol in a for loop.",
      type = "warning"
    )
  })
}
