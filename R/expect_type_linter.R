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

    base_type_tests <- xp_text_in_table(paste0("is.", base_types)) # nolint: object_usage_linter. TODO(#942): fix this.
    xpath <- glue::glue("//expr[
      (
        SYMBOL_FUNCTION_CALL[text() = 'expect_equal' or text() = 'expect_identical']
        and following-sibling::expr[
          expr[SYMBOL_FUNCTION_CALL[text() = 'typeof']]
          and (position() = 1 or preceding-sibling::expr[STR_CONST])
        ]
      ) or (
        SYMBOL_FUNCTION_CALL[text() = 'expect_true']
        and following-sibling::expr[1][expr[SYMBOL_FUNCTION_CALL[ {base_type_tests} ]]]
      )
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)
    return(lapply(bad_expr, gen_expect_type_lint, source_file))
  })
}

gen_expect_type_lint <- function(expr, source_file) {
  matched_function <- xml2::xml_text(xml2::xml_find_first(expr, "SYMBOL_FUNCTION_CALL"))
  if (matched_function %in% c("expect_equal", "expect_identical")) {
    lint_msg <- sprintf("expect_type(x, t) is better than %s(typeof(x), t)", matched_function)
  } else {
    lint_msg <- "expect_type(x, t) is better than expect_true(is.<t>(x))"
  }
  xml_nodes_to_lint(expr, source_file, lint_msg, type = "warning")
}

# NB: the full list of values that can arise from `typeof(x)` is available
#   in ?typeof (or, slightly more robustly, in the R source: src/main/util.c.
#   Not all of them are available in is.<type> form, e.g. 'any' or
#   'special'. 'builtin' and 'closure' are special cases, corresponding to
#   is.primitive and is.function (essentially).
base_types <- c(
  "raw", "logical", "integer", "double", "complex", "character", "list",
  "numeric", "function", "primitive", "environment", "pairlist", "promise",
  # Per ?is.language, it's the same as is.call || is.name || is.expression.
  #   so by blocking it, we're forcing more precise tests of one of
  #   those directly ("language", "symbol", and "expression", resp.)
  # NB: is.name and is.symbol are identical.
  "language", "call", "name", "symbol", "expression"
)
