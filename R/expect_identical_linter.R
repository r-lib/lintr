#' Require usage of `expect_identical(x, y)` where appropriate
#'
#' This linter enforces the usage of [testthat::expect_identical()] as the
#'   default expectation for comparisons in a testthat suite. `expect_true(identical(x, y))`
#'   is an equivalent but unadvised method of the same test. Further,
#'   [testthat::expect_equal()] should only be used when `expect_identical()`
#'   is inappropriate, i.e., when `x` and `y` need only be *numerically
#'   equivalent* instead of fully identical (in which case, provide the
#'   `tolerance=` argument to `expect_equal()` explicitly). This also applies
#'   when it's inconvenient to check full equality (e.g., names can be ignored,
#'   in which case `ignore_attr = "names"` should be supplied to
#'   `expect_equal()` (or, for 2nd edition, `check.attributes = FALSE`).
#'
#' @section Exceptions:
#'
#' The linter allows `expect_equal()` in three circumstances:
#'   1. A named argument is set (e.g. `ignore_attr` or `tolerance`)
#'   2. Comparison is made to an explicit decimal, e.g.
#'      `expect_equal(x, 1.0)` (implicitly setting `tolerance`)
#'   3. `...` is passed (wrapper functions which might set
#'      arguments such as `ignore_attr` or `tolerance`)
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_equal(x, y)",
#'   linters = expect_identical_linter()
#' )
#'
#' lint(
#'   text = "expect_true(identical(x, y))",
#'   linters = expect_identical_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "expect_identical(x, y)",
#'   linters = expect_identical_linter()
#' )
#'
#' lint(
#'   text = "expect_equal(x, y, check.attributes = FALSE)",
#'   linters = expect_identical_linter()
#' )
#'
#' lint(
#'   text = "expect_equal(x, y, tolerance = 1e-6)",
#'   linters = expect_identical_linter()
#' )
#'
#' @evalRd rd_tags("expect_identical_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_identical_linter <- function() {
  # outline:
  #   - skip when any named argument is set. most commonly this
  #     is check.attributes (for 2e tests) or one of the ignore_*
  #     arguments (for 3e tests). This will generate some false
  #     negatives, but will be much easier to maintain.
  #   - skip cases like expect_equal(x, 1.02) or the constant vector version
  #     where a numeric constant indicates inexact testing is preferable
  #   - skip calls using dots (`...`); see tests
  non_integer <- glue::glue("
    NUM_CONST[contains(text(), '.')]
    or (
      OP-MINUS
      and count(expr) = 1
      and expr[NUM_CONST[contains(text(), '.')]]
    )
  ")

  expect_equal_xpath <- glue::glue("
  self::*[not(
      following-sibling::EQ_SUB
      or following-sibling::expr[
        (
          expr[1][SYMBOL_FUNCTION_CALL[text() = 'c']]
          and expr[{non_integer}]
        ) or (
          {non_integer}
        ) or (
          OP-MINUS
          and count(expr) = 1
          and expr[
            expr[1][SYMBOL_FUNCTION_CALL[text() = 'c']]
            and expr[{non_integer}]
          ]
        ) or (
          SYMBOL[text() = '...']
        )
      ]
    )]
    /parent::expr
  ")
  expect_true_xpath <- "
  following-sibling::expr[1][expr[1]/SYMBOL_FUNCTION_CALL[text() = 'identical']]
    /parent::expr
  "
  Linter(linter_level = "expression", function(source_expression) {
    expect_equal_calls <- source_expression$xml_find_function_calls("expect_equal")
    expect_true_calls <- source_expression$xml_find_function_calls("expect_true")
    bad_expr <- c(
      xml_find_all(expect_equal_calls, expect_equal_xpath),
      xml_find_all(expect_true_calls, expect_true_xpath)
    )

    xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = paste(
        "Use expect_identical(x, y) by default; resort to expect_equal() only when needed,",
        "e.g. when setting ignore_attr= or tolerance=."
      ),
      type = "warning"
    )
  })
}
