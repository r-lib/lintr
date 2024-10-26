#' Complex Conditional Expressions Linter
#'
#' Detects complex conditional expressions and suggests extracting
#' them into Boolean functions or variables for improved readability and reusability.
#'
#' For example, if you have a conditional expression with more than two logical operands,
#'
#' ```
#' if (looks_like_a_duck(x) &&
#'     swims_like_a_duck(x) &&
#'     quacks_like_a_duck(x)) {
#'     ...
#' }
#' ````
#'
#' to improve its readability and reusability, you can extract the conditional expression.
#'
#' You can either extract it into a Boolean function:
#'
#' ```
#' is_duck <- function(x) {
#'   looks_like_a_duck(x) &&
#'     swims_like_a_duck(x) &&
#'     quacks_like_a_duck(x)
#' }
#'
#' if (is_duck(x)) {
#'   ...
#' }
#' ```
#'
#' or into a Boolean variable:
#'
#' ```
#' is_duck <- looks_like_a_duck(x) &&
#'    swims_like_a_duck(x) &&
#'    quacks_like_a_duck(x)
#'
#' if (is_duck) {
#'  ...
#' }
#' ```
#'
#' In addition to improving code readability, extracting complex conditional expressions has the added benefit
#' of introducing a reusable abstraction.
#'
#' @param threshold Integer. The maximum number of logical operators (`&&` or `||`)
#'   allowed in a conditional expression. The default is `1L`, meaning any conditional expression
#'   with more than one logical operator will be flagged.
#'
#' @examples
#' # will produce lints
#' code <- "if (a && b && c) { do_something() }"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = complex_conditional_linter()
#' )
#'
#' # okay
#' ready_to_do_something <- a && b && c
#' code <- "if (ready_to_do_something) { do_something() }"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = complex_conditional_linter()
#' )
#'
#' @evalRd rd_tags("complex_conditional_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
complex_conditional_linter <- function(threshold = 1L) {
  stopifnot(is.integer(threshold), length(threshold) == 1L, threshold >= 1L)

  xpath <- glue::glue("//expr[
    count(descendant-or-self::expr[count(expr) > {threshold} and (AND2 or OR2)]) > 1
    and
    not(ancestor::expr[count(descendant-or-self::expr[count(expr) > {threshold} and (AND2 or OR2)]) > 1])
  ]")

  Linter(linter_level = "expression", function(source_expression) {
    xml <- source_expression$xml_parsed_content

    nodes <- xml2::xml_find_all(xml, xpath)

    lints <- xml_nodes_to_lints(
      nodes,
      source_expression = source_expression,
      lint_message = paste0(
        "Complex conditional with more than ",
        threshold,
        " logical operator(s). Consider extracting into a boolean function or variable for readability and reusability."
      ),
      type = "warning"
    )

    lints
  })
}
