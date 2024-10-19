#' Complex Conditional Expressions Linter
#'
#' Detects complex conditional expressions and suggests extracting
#' them into Boolean functions or variables for improved readability and reusability.
#'
#' @param threshold Integer. The maximum number of logical operands (`&&` or `||`)
#'   allowed in a conditional expression (default: `2L`).
#'
#' @examples
#' # This will produce a lint because there are three logical operands
#' code <- "if (a && b && c) { do_something() }"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = complex_conditional_linter()
#' )
#'
#' # This will not produce a lint because it meets the threshold
#' code <- "if (a && b) { do_something() }"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = complex_conditional_linter()
#' )
#'
#' @evalRd rd_tags("complex_conditional_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
complex_conditional_linter <- function(threshold = 2L) {
  stopifnot(is.integer(threshold), length(threshold) == 1L, threshold >= 1L)

  xpath <- glue::glue("
    //IF | //WHILE
    [
      count(.//AND | .//OR) > {threshold - 1}
    ]
  ")

  Linter(function(source_expression) {
    xml <- source_expression$xml_parsed_content

    nodes <- xml2::xml_find_all(xml, xpath)

    lints <- xml_nodes_to_lints(
      nodes,
      source_expression = source_expression,
      lint_message = paste0(
        "Complex conditional with more than ",
        threshold,
        " logical operands. Consider extracting into a boolean function or variable for readability and reusability."
      ),
      type = "warning"
    )

    lints
  })
}
