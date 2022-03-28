#' Force && conditions in expect_true(), expect_false() to be written separately
#'
#' For readability of test outputs, testing only one thing per call to
#'   [testthat::expect_true()] is preferable, i.e.,
#'   `expect_true(A); expect_true(B)` is better than `expect_true(A && B)`, and
#'   `expect_false(A); expect_false(B)` is better than `expect_false(A || B)`.
#'
#' @evalRd rd_tags("conjunct_test_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
conjunct_test_linter <- function() {
  Linter(function(source_file) {
    # need the full file to also catch usages at the top level
    if (length(source_file$full_parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$full_xml_parsed_content

    # TODO(michaelchirico): address keyword arguments in R>=4.0 for stopifnot(). lint optionally (on by default)?
    xpath <- "//expr[
      (
        expr[SYMBOL_FUNCTION_CALL[text() = 'expect_true' or text() = 'stopifnot']]
        and expr[2][AND2]
      ) or (
        expr[SYMBOL_FUNCTION_CALL[text() = 'expect_false']]
        and expr[2][OR2]
      )
    ]"

    bad_expr <- xml2::xml_find_all(xml, xpath)

    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file,
      lint_message = function(expr) {
        matched_fun <- xml2::xml_text(xml2::xml_find_first(expr, "expr/SYMBOL_FUNCTION_CALL"))
        op <- xml2::xml_text(xml2::xml_find_first(expr, "expr/*[self::AND2 or self::OR2]"))
        instead_of <- sprintf("Instead of %s(A %s B),", matched_fun, op)
        if (matched_fun %in% c("expect_true", "expect_false")) {
          replacement <- sprintf("write multiple expectations like %1$s(A) and %1$s(B)", matched_fun)
        } else {
          replacement <- "write multiple conditions like stopifnot(A, B)."
        }
        paste(instead_of, replacement, "The latter will produce better error messages in the case of failure.")
      },
      type = "warning",
      global = TRUE
    ))
  })
}
