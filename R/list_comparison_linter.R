#' Block usage of comparison operators with known-list() functions like lapply
#'
#' Usage like `lapply(x, sum) > 10` is awkward because the list must first
#'   be coerced to a vector for comparison. A function like [vapply()]
#'   should be preferred.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "lapply(x, sum) > 10",
#'   linters = list_comparison_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "unlist(lapply(x, sum)) > 10",
#'   linters = list_comparison_linter()
#' )
#'
#' @evalRd rd_tags("list_comparison_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
list_comparison_linter <- function() {
  # TODO(michaelchirico): extend to cases where using simplify=FALSE implies a
  #   list output, e.g. with sapply, replicate, mapply.
  list_mapper_alternatives <- c(
    lapply = "vapply(x, FUN, character(1L))",
    map = "map_chr(x, FUN)",
    Map = "mapply()",
    .mapply = "mapply()"
  )

  # NB: anchor to the comparison expr so that we can easily include the comparator
  #   in the lint message.
  xpath <- glue("
  //SYMBOL_FUNCTION_CALL[{ xp_text_in_table(names(list_mapper_alternatives)) }]
    /parent::expr
    /parent::expr
    /parent::expr[{ xp_or(infix_metadata$xml_tag[infix_metadata$comparator]) }]
  ")

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    bad_expr <- xml_find_all(xml, xpath)

    list_mapper <- xp_call_name(bad_expr, depth = 2L)

    vector_mapper <- list_mapper_alternatives[list_mapper]
    # we are at `x ? y` in which the comparator ? comes 2nd
    comparator <- xml_find_chr(bad_expr, "string(*[2])")

    lint_message <- as.character(glue(
      "The output of {list_mapper}(), a list(), is being ",
      "coerced for comparison by `{comparator}`. ",
      "Instead, use a mapper that generates a vector with the correct type ",
      "directly, for example {vector_mapper} if the output is a string."
    ))
    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = lint_message,
      type = "warning"
    )
  })
}