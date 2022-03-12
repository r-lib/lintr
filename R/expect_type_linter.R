#' Require usage of expect_type(x, type) over expect_equal(typeof(x), type)
#'
#' [testthat::expect_type()] exists specifically for testing the storage type
#'   of objects. [testthat::expect_equal()], [testthat::expect_identical()], and
#'   [testthat::expect_true()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @evalRd rd_tags("expect_type_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_type_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }
  
    xml <- source_file$xml_parsed_content
  
    # for expect_{equal,identical}(typeof(x), t)
    expect_calls <- xp_text_in_table(c("expect_equal", "expect_identical"))
    equal_expr_cond <- xp_and(
      sprintf("SYMBOL_FUNCTION_CALL[%s]", expect_calls),
      "following-sibling::expr[1][expr[SYMBOL_FUNCTION_CALL[text() = 'typeof']]]"
    )
  
    # for expect_true(is.<type>(x))
    true_expr_cond <- xp_and(
      "SYMBOL_FUNCTION_CALL[text() = 'expect_true']",
      sprintf(
        "following-sibling::expr[1][expr[SYMBOL_FUNCTION_CALL[%s]]]",
        xp_text_in_table(paste0("is.", kBaseTypes))
      )
    )
  
    xpath <- sprintf("//expr[(%s) or (%s)]", equal_expr_cond, true_expr_cond)
  
    bad_expr <- xml2::xml_find_all(xml, xpath)
    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      message = paste(
        "expect_type(x, t) is better than expect_equal(typeof(x), t),",
        "expect_identical(typeof(x), t), or expect_true(is.<t>(x)).",
        'Note that typeof(1) is "double" and typeof(function(x) x) is "closure".'
      ),
      type = "warning"
    ))
  })
}
