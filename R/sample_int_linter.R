#' Require usage of sample.int(n, m, ...) over sample(1:n, m, ...)
#'
#' [sample.int()] is preferable to `sample()` for the case of sampling numbers
#'   between 1 and `n`. `sample` calls `sample.int()` "under the hood".
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "sample(1:10, 2)",
#'   linters = sample_int_linter()
#' )
#'
#' lint(
#'   text = "sample(seq(4), 2)",
#'   linters = sample_int_linter()
#' )
#'
#' lint(
#'   text = "sample(seq_len(8), 2)",
#'   linters = sample_int_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "sample(seq(1, 5, by = 2), 2)",
#'   linters = sample_int_linter()
#' )
#'
#' lint(
#'   text = "sample(letters, 2)",
#'   linters = sample_int_linter()
#' )
#'
#' @evalRd rd_tags("sample_int_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
sample_int_linter <- function() {
  # looking for anything like sample(1: that doesn't come after a $ extraction
  # exclude TRUE/FALSE for sample(replace = TRUE, ...) usage. better
  #   would be match.arg() but this also works.
  xpath <- glue("
  self::*[not(OP-DOLLAR or OP-AT)]
    /following-sibling::expr[1][
      (
        expr[1]/NUM_CONST[text() = '1' or text() = '1L']
        and OP-COLON
      )
      or expr/SYMBOL_FUNCTION_CALL[text() = 'seq_len']
      or (
        expr/SYMBOL_FUNCTION_CALL[text() = 'seq']
        and (
          count(expr) = 2
          or (
            expr[2]/NUM_CONST[text() = '1' or text() = '1L']
            and not(SYMBOL_SUB[
              text() = 'by'
              and not(following-sibling::expr[1]/NUM_CONST[text() = '1' or text() = '1L'])
            ])
          )
        )
      )
      or NUM_CONST[not(text() = 'TRUE' or text() = 'FALSE')]
    ]
    /parent::expr
  ")

  Linter(linter_level = "expression", function(source_expression) {
    xml_calls <- source_expression$xml_find_function_calls("sample")
    bad_expr <- xml_find_all(xml_calls, xpath)

    first_call <- xp_call_name(bad_expr, depth = 2L)
    original <- sprintf("%s(n)", first_call)
    original[!is.na(xml_find_first(bad_expr, "expr[2]/OP-COLON"))] <- "1:n"
    original[!is.na(xml_find_first(bad_expr, "expr[2]/NUM_CONST"))] <- "n"

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = glue("sample.int(n, m, ...) is preferable to sample({original}, m, ...)."),
      type = "warning"
    )
  })
}
