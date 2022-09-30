#' Require usage of boolean operators over equivalent arithmetic
#'
#' `length(which(x == y)) == 0` is the same as `!any(x == y)`, but the latter
#'   is more readable and more efficient.
#'
#' #' @evalRd rd_tags("boolean_arithmetic_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
boolean_arithmetic_linter <- function() {
  # TODO(michaelchirico): consider sum(x %in% y)
  # TODO(michaelchirico): consider sum(A & B), sum(A | B), and sum(!A)
  # TODO(michaelchirico): consider is.na, is.nan, is.finite, is.infinite, is.element
  # TODO(michaelchirico): stringr equivalents to regex functions
  zero_expr <-
    "(EQ or NE or GT or LE) and expr[NUM_CONST[text() = '0' or text() = '0L']]"
  one_expr <-
    "(LT or GE) and expr[NUM_CONST[text() = '1' or text() = '1L']]"
  length_xpath <- glue::glue("
    //SYMBOL_FUNCTION_CALL[text() = 'which' or text() = 'grep']
    /parent::expr
    /parent::expr
    /parent::expr[
      expr[SYMBOL_FUNCTION_CALL[text() = 'length']]
      and parent::expr[ ({zero_expr}) or ({one_expr})]
    ]
  ")
  sum_xpath <- glue::glue("
    //SYMBOL_FUNCTION_CALL[text() = 'sum']
    /parent::expr
    /parent::expr[
      expr[
        expr[SYMBOL_FUNCTION_CALL[text() = 'grepl']]
        or (EQ or NE or GT or LT or GE or LE)
      ] and parent::expr[ ({zero_expr}) or ({one_expr})]
    ]
  ")
  any_xpath <- paste(length_xpath, "|", sum_xpath)

  Linter(function(source_expression) {
    if (!is_lint_level(source_expression, "expression")) {
      return(list())
    }

    xml <- source_expression$xml_parsed_content

    any_expr <- xml2::xml_find_all(xml, any_xpath)

    # TODO(michaelchirico): extend to include all()-alike expressions like
    #   sum(x == y) == length(x) --> all(x == y). The issues are (1) we can't
    #   just test for length() on the RHS (e.g. `sum(x == y) == length(z)` could
    #   be totally different); (2) looking for sum(x == y) == length(x == y)
    #   will only find a particularly silly manifestation of the issue; and
    #   (3) there is a bit of a combinatorial explosion if we have to look for
    #   _any_ symbol found inside the logical expression on the RHS
    #   (e.g. sum(x == y) == [length(x) _or_ length(y)]).
    # A first pass would be to check how many hits there are for the naive
    #   version (i.e., condition 1), to see how worth investing in this
    #   complicated logic is to begin with.

    xml_nodes_to_lints(
      any_expr,
      source_expression = source_expression,
      # TODO(michaelchirico): customize this?
      lint_message = paste(
        "Use any() to express logical aggregations.",
        "For example, replace length(which(x == y)) == 0 with !any(x == y)."
      ),
      type = "warning"
    )
  })
}
