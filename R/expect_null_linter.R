#' expect_null Linter
#'
#' Require usage of `expect_null(x)` over `expect_equal(x, NULL)` and similar
#' usages.
#'
#' [testthat::expect_null()] exists specifically for testing for `NULL` objects.
#' [testthat::expect_equal()], [testthat::expect_identical()], and
#' [testthat::expect_true()] can also be used for such tests,
#' but it is better to use the tailored function instead.
#'
#' @evalRd rd_tags("expect_null_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_null_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # two cases two match:
    #  (1) expect_{equal,identical}(x, NULL)
    #  (2) expect_true(is.null(x))
    xpath <- glue::glue("//expr[
      (
        SYMBOL_FUNCTION_CALL[ {xp_text_in_table(c('expect_equal', 'expect_identical'))} ]
        and following-sibling::expr[position() <= 2 and NULL_CONST]
      ) or (
        SYMBOL_FUNCTION_CALL[text() = 'expect_true']
        and following-sibling::expr[1][expr[SYMBOL_FUNCTION_CALL[text() = 'is.null']]]
      )
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)

    lapply(bad_expr, gen_expect_null_lint, source_file)
  })
}

gen_expect_null_lint <- function(expr, source_file) {
  matched_function <- xml2::xml_text(xml2::xml_find_first(expr, "SYMBOL_FUNCTION_CALL"))
  if (matched_function %in% c("expect_equal", "expect_identical")) {
    lint_msg <- sprintf("expect_null(x) is better than %s(x, NULL)", matched_function)
  } else {
    lint_msg <- "expect_null(x) is better than expect_true(is.null(x))"
  }
  xml_nodes_to_lint(expr, source_file, lint_msg, type = "warning")
}
