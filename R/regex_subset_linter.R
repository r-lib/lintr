#' Require usage of direct methods for subsetting strings via regex
#'
#' Using `value = TRUE` in [grep()] returns the subset of the input that matches
#'   the pattern, e.g. `grep("[a-m]", letters, value = TRUE)` will return the
#'   first 13 elements (`a` through `m`).
#'
#' `letters[grep("[a-m]", letters)]` and `letters[grepl("[a-m]", letters)]`
#'   both return the same thing, but more circuitously and more verbosely.
#'
#' The `stringr` package also provides an even more readable alternative,
#'   namely `str_subset()`, which should be preferred to versions using
#'   `str_detect()` and `str_which()`.
#'
#' @section Exceptions:
#'   Note that `x[grep(pattern, x)]` and `grep(pattern, x, value = TRUE)`
#'   are not _completely_ interchangeable when `x` is not character
#'   (most commonly, when `x` is a factor), because the output of the
#'   latter will be a character vector while the former remains a factor.
#'   It still may be preferable to refactor such code, as it may be faster
#'   to match the pattern on `levels(x)` and use that to subset instead.
#'
#' @evalRd rd_tags("regex_subset_linter")
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x[grep(pattern, x)]",
#'   linters = regex_subset_linter()
#' )
#'
#' lint(
#'   text = "x[stringr::str_which(x, pattern)]",
#'   linters = regex_subset_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "grep(pattern, x, value = TRUE)",
#'   linters = regex_subset_linter()
#' )
#'
#' lint(
#'   text = "stringr::str_subset(x, pattern)",
#'   linters = regex_subset_linter()
#' )
#'
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
regex_subset_linter <- function() {
  xpath_fmt <- "
  self::*[
    OP-LEFT-BRACKET
    and not(parent::*[LEFT_ASSIGN or EQ_ASSIGN or RIGHT_ASSIGN])
    and expr[1] = expr/expr[position() = {arg_pos} ]
  ]"
  grep_xpath <- glue(xpath_fmt, arg_pos = 3L)
  stringr_xpath <- glue(xpath_fmt, arg_pos = 2L)

  Linter(linter_level = "expression", function(source_expression) {
    grep_calls <- xml_parent(xml_parent(
      source_expression$xml_find_function_calls(c("grepl", "grep"))
    ))
    grep_calls <- strip_comments_from_subtree(grep_calls)
    grep_expr <- xml_find_all(grep_calls, grep_xpath)

    grep_lints <- xml_nodes_to_lints(
      grep_expr,
      source_expression = source_expression,
      lint_message =
        "Prefer grep(pattern, x, ..., value = TRUE) over x[grep(pattern, x, ...)] and x[grepl(pattern, x, ...)].",
      type = "warning"
    )

    stringr_calls <- xml_parent(xml_parent(
      source_expression$xml_find_function_calls(c("str_detect", "str_which"))
    ))
    stringr_calls <- strip_comments_from_subtree(stringr_calls)
    stringr_expr <- xml_find_all(stringr_calls, stringr_xpath)

    stringr_lints <- xml_nodes_to_lints(
      stringr_expr,
      source_expression = source_expression,
      lint_message =
        "Prefer stringr::str_subset(x, pattern) over x[str_detect(x, pattern)] and x[str_which(x, pattern)].",
      type = "warning"
    )

    c(grep_lints, stringr_lints)
  })
}
