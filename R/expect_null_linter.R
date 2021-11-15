#' @describeIn google-linters Require usage of expect_null(x) over expect_equal(x, NULL) and similar
#' [testthat::expect_null()] exists specifically for testing for `NULL` objects.
#'   [testthat::expect_equal()], [testthat::expect_identical()], and
#'   [testthat::expect_true()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#' @export
expect_null_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }
  
    xml <- source_file$xml_parsed_content
  
    # for expect_{equal,identical}(x, NULL)
    equal_expr_cond <- sprintf(
      "SYMBOL_FUNCTION_CALL[%s] and following-sibling::expr[2][NULL_CONST]",
      xp_text_in_table(c("expect_equal", "expect_identical"))
    )
  
    # for expect_true(is.null(x))
    is_null_call <- "SYMBOL_FUNCTION_CALL[text() = 'is.null']"
    true_expr_cond <- xp_and(
      "SYMBOL_FUNCTION_CALL[text() = 'expect_true']",
      sprintf("following-sibling::expr[1][expr[%s]]", is_null_call)
    )
  
    xpath <- sprintf("//expr[(%s) or (%s)]", equal_expr_cond, true_expr_cond)
  
    bad_expr <- xml2::xml_find_all(xml, xpath)
  
    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = paste(
        "expect_null(x) is better than expect_equal(x, NULL),",
        "expect_identical(x, NULL), or expect_true(is.null(x))."
      ),
      type = "warning"
    ))
  })
}
