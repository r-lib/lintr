#' Require usage of `expect_false(x)` over `expect_true(!x)`
#'
#' [testthat::expect_false()] exists specifically for testing that an output is
#'   `FALSE`. [testthat::expect_true()] can also be used for such tests by
#'   negating the output, but it is better to use the tailored function instead.
#'   The reverse is also true -- use `expect_false(A)` instead of
#'   `expect_true(!A)`.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "expect_true(!x)",
#'   linters = expect_not_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "expect_false(x)",
#'   linters = expect_not_linter()
#' )
#'
#' @evalRd rd_tags("expect_not_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
expect_not_linter <- make_linter_from_function_xpath(
  function_names = c("expect_true", "expect_false"),
  xpath = "
  following-sibling::expr[OP-EXCLAMATION]
    /parent::expr
  ",
  lint_message = "expect_false(x) is better than expect_true(!x), and vice versa."
)
