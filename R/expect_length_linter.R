#' Require usage of expect_length(x, n) over expect_equal(length(x), n)
#'
#' [testthat::expect_length()] exists specifically for testing the [length()] of
#'   an object. [testthat::expect_equal()] can also be used for such tests,
#'   but it is better to use the tailored function instead.
#'
#' @evalRd rd_tags("expect_length_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_length_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # TODO(michaelchirico): also catch expect_true(length(x) == 1)
    xpath <- sprintf("//expr[
      SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical']
      and following-sibling::expr[
        expr[SYMBOL_FUNCTION_CALL[text() = 'length']]
        and (position() = 1 or preceding-sibling::expr[NUM_CONST])
      ]
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)
    return(lapply(bad_expr, gen_expect_length_lint, source_file))
  })
}

gen_expect_length_lint <- function(expr, source_file) {
  matched_function <- xml2::xml_text(xml2::xml_find_first(expr, "SYMBOL_FUNCTION_CALL"))
  lint_msg <- sprintf("expect_length(x, n) is better than %s(length(x), n)", matched_function)
  xml_nodes_to_lint(expr, source_file, lint_msg, type = "warning")
}
