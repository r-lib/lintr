#' Warn about invalid usage of `all.equal()`
#'
#' [all.equal()] returns `TRUE` in the absence of differences but return a
#' character string (not `FALSE`) in the presence of differences.
#' Usage of `all.equal()` without wrapping it in `isTRUE()` in `if` clauses, or
#' preceded by the negation operator `!`, are thus likely to generate unexpected
#' errors if the compared objects have differences.
#' An alternative is to use `identical()` to compare vector of strings or when
#' exact equality is expected.
#'
#' @examples
#' # lints
#' lint(
#'   text = 'if (all.equal(a, b)) message("equal")',
#'   linters = all_equal_linter()
#' )
#'
#' lint(
#'   text = '!all.equal(a, b)',
#'   linters = all_equal_linter()
#' )
#'
#' # okay
#' lint(
#'   text = 'if (isTRUE(all.equal(a, b))) message("equal")',
#'   linters = all_equal_linter()
#' )
#'
#' lint(
#'   text = '!identical(a, b)',
#'   linters = all_equal_linter()
#' )
#'
#' @evalRd rd_tags("all_equal_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
all_equal_linter <- function() {

  Linter(linter_level = "expression", function(source_expression) {
    all_equal_calls <- source_expression$xml_find_function_calls("all.equal")

    in_if <- xml_find_all(
      all_equal_calls,
      "parent::expr[preceding-sibling::IF]"
    )
    negated <- xml_find_all(
      all_equal_calls,
      "parent::expr[preceding-sibling::OP-EXCLAMATION]"
    )

    xml_nodes_to_lints(
      combine_nodesets(in_if, negated),
      source_expression = source_expression,
      lint_message = "Wrap all.equal() in isTRUE(), or replace it by identical() if no tolerance is required.",
      type = "warning"
    )
  })
}
