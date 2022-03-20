#' Require usage of expect_identical(x, y) where appropriate
#'
#' At Google, [testthat::expect_identical()] should be the default/go-to function for
#'   comparing an output to an expected value. `expect_true(identical(x, y))`
#'   is an equivalent but unadvised method of the same test. Further,
#'   [testthat::expect_equal()] should only be used when `expect_identical()`
#'   is inappropriate, i.e., when `x` and `y` need only be *numerically
#'   equivalent* instead of fully identical (in which case, provide the
#'   `tolerance=` argument to `expect_equal()` explicitly). This also applies
#'   when it's inconvenient to check full equality (e.g., names can be ignored,
#'   in which case `ignore_attr = "names"` should be supplied to
#'   `expect_equal()` (or, for 2nd edition, `check.attributes = FALSE`).
#'
#' NB: The linter allows `expect_equal()` in three circumstances:
#'   1. A named argument is set (e.g. `ignore_attr` or `tolerance`)
#'   2. Comparison is made to an explicit decimal, e.g.
#'      `expect_equal(x, 1.0)` (implicitly setting `tolerance`)
#'   3. `...` is passed (wrapper functions whcih might set
#'      arguments such as `ignore_attr` or `tolerance`)
#'
#' @evalRd rd_tags("expect_identical_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_identical_linter <- function() {
  Linter(function(source_file) {
    if (length(source_file$parsed_content) == 0L) {
      return(list())
    }

    xml <- source_file$xml_parsed_content

    # outline:
    #   1. conditions for expect_equal()
    #     - skip when any named argument is set. most commonly this
    #       is check.attributes (for 2e tests) or one of the ignore_*
    #       arguments (for 3e tests). This will generate some false
    #       negatives, but will be much easier to maintain.
    #     - skip cases like expect_equal(x, 1.02) or the constant vector version
    #       where a numeric constant indicates inexact testing is preferable
    #     - skip calls using dots (`...`); see tests
    #   2. conditions for expect_true()
    xpath <- glue::glue("//expr[
      (
        SYMBOL_FUNCTION_CALL[text() = 'expect_equal']
        and not(
          following-sibling::SYMBOL_SUB
          or following-sibling::expr[
            expr[SYMBOL_FUNCTION_CALL[text() = 'c']]
            and expr[NUM_CONST[contains(text(), '.')]]
          ]
          or following-sibling::expr[NUM_CONST[contains(text(), '.')]]
          or following-sibling::expr[SYMBOL[text() = '...']]
        )
      ) or (
        SYMBOL_FUNCTION_CALL[text() = 'expect_true']
        and following-sibling::expr[1][
          expr[SYMBOL_FUNCTION_CALL[text() = 'identical']]
        ]
      )
    ]")

    bad_expr <- xml2::xml_find_all(xml, xpath)
    return(lapply(
      bad_expr,
      xml_nodes_to_lint,
      source_file = source_file,
      lint_message = paste(
        "Use expect_identical(x, y) by default; resort to expect_equal() only when needed,",
        "e.g. when setting ignore_attr= or tolerance=."
      ),
      type = "warning"
    ))
  })
}
