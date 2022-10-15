#' Force `&&` conditions in `expect_true()` and `expect_false()` to be written separately
#'
#' For readability of test outputs, testing only one thing per call to
#'   [testthat::expect_true()] is preferable, i.e.,
#'   `expect_true(A); expect_true(B)` is better than `expect_true(A && B)`, and
#'   `expect_false(A); expect_false(B)` is better than `expect_false(A || B)`.
#'
#' Similar reasoning applies to `&&` usage inside [stopifnot()] and `assertthat::assert_that()` calls.
#'
#' @param allow_named_stopifnot Logical, `TRUE` by default. If `FALSE`, "named" calls to `stopifnot()`,
#'   available since R 4.0.0 to provide helpful messages for test failures, are also linted.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_true(x && y)",
#'   linters = conjunct_test_linter()
#' )
#'
#' lint(
#'   text = "expect_false(x || (y && z))",
#'   linters = conjunct_test_linter()
#' )
#'
#' lint(
#'   text = "stopifnot('x must be a logical scalar' = length(x) == 1 && is.logical(x) && !is.na(x))",
#'   linters = conjunct_test_linter(allow_named_stopifnot = FALSE)
#' )
#'
#' # okay
#' lint(
#'   text = "expect_true(x || (y && z))",
#'   linters = conjunct_test_linter()
#' )
#'
#' lint(
#'   text = 'stopifnot("x must be a logical scalar" = length(x) == 1 && is.logical(x) && !is.na(x))',
#'   linters = conjunct_test_linter(allow_named_stopifnot = TRUE)
#' )
#'
#' @evalRd rd_tags("conjunct_test_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
conjunct_test_linter <- function(allow_named_stopifnot = TRUE) {
  expect_true_assert_that_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'expect_true' or text() = 'assert_that']
  /parent::expr
  /following-sibling::expr[1][AND2]
  "
  named_stopifnot_condition <- if (allow_named_stopifnot) "and not(preceding-sibling::*[1][self::EQ_SUB])" else ""
  stopifnot_xpath <- glue::glue("
  //SYMBOL_FUNCTION_CALL[text() = 'stopifnot']
  /parent::expr
  /following-sibling::expr[1][AND2 {named_stopifnot_condition}]
  ")
  expect_false_xpath <- "
  //SYMBOL_FUNCTION_CALL[text() = 'expect_false']
  /parent::expr
  /following-sibling::expr[1][OR2]
  "
  xpath <- paste0(
    c(expect_true_assert_that_xpath, stopifnot_xpath, expect_false_xpath),
    "/parent::expr",
    collapse = " | "
  )

  Linter(function(source_expression) {
    # need the full file to also catch usages at the top level
    if (!is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    if (length(bad_expr) == 0L) {
      return(list())
    }

    matched_fun <- xp_call_name(bad_expr)
    operator <- xml2::xml_find_chr(bad_expr, "string(expr/*[self::AND2 or self::OR2])")
    replacement_fmt <- ifelse(
      matched_fun %in% c("expect_true", "expect_false"),
      "write multiple expectations like %1$s(A) and %1$s(B)",
      "write multiple conditions like %s(A, B)."
    )
    lint_message <- paste(
      sprintf("Instead of %s(A %s B),", matched_fun, operator),
      sprintf(replacement_fmt, matched_fun),
      "The latter will produce better error messages in the case of failure."
    )
    xml_nodes_to_lints(bad_expr, source_expression, lint_message = lint_message, type = "warning")
  })
}
