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
#' lint(
#'   text = 'isFALSE(all.equal(a, b))',
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
#' lint(
#'   text = "!isTRUE(all.equal(a, b))",
#'   linters = all_equal_linter()
#' )
#'
#' @evalRd rd_tags("all_equal_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
all_equal_linter <- function() {

  Linter(linter_level = "expression", function(source_expression) {
    all_equal_calls <- source_expression$xml_find_function_calls("all.equal")

    dangerous_unwrapped_all_equal <- xml_find_all(
      all_equal_calls,
      "parent::expr[
        preceding-sibling::OP-EXCLAMATION
        or preceding-sibling::IF
        or preceding-sibling::WHILE
      ]"
    )

    has_tolerance_arg <- !is.na(
      xml_find_first(dangerous_unwrapped_all_equal, "SYMBOL_SUB[text() = 'tolerance']")
    )

    unwrapped_all_equal_lints <- xml_nodes_to_lints(
      dangerous_unwrapped_all_equal,
      source_expression = source_expression,
      lint_message = ifelse(
        has_tolerance_arg,
        "Wrap all.equal() in isTRUE().",
        "Wrap all.equal() in isTRUE(), or replace it by identical() if no tolerance is required."
      ),
      type = "warning"
    )

    is_false_all_equal <- xml_find_all(
      all_equal_calls,
      "parent::expr[preceding-sibling::expr[1]/SYMBOL_FUNCTION_CALL/text() = 'isFALSE']"
    )

    is_false_all_equal_lints <- xml_nodes_to_lints(
      is_false_all_equal,
      source_expression = source_expression,
      lint_message = paste(
        "Use !isTRUE() to check for differences in all.equal().",
        "isFALSE(all.equal()) always returns FALSE."
      ),
      type = "warning"
    )

    c(unwrapped_all_equal_lints, is_false_all_equal_lints)
  })
}
