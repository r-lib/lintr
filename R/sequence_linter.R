#' Require usage of `sequence()` where possible
#'
#' [sequence()] is a base R function equivalent to `unlist(lapply(x, seq_len))`,
#' but faster and more readable.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "unlist(sapply(x, seq_len))",
#'   linters = sequence_linter()
#' )
#'
#' lint(
#'   text = "unlist(lapply(x, seq_len))",
#'   linters = sequence_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "sapply(x, seq_len)",
#'   linters = lengths_linter()
#' )
#'
#' @evalRd rd_tags("sequence_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
sequence_linter <- make_linter_from_function_xpath(
  # only a small subset of loop functionals need to be considered because
  # seq_len() output will not have constant length
  function_names = c("sapply", "lapply", "map"),
  xpath = "parent::expr/parent::expr[
    preceding-sibling::expr/SYMBOL_FUNCTION_CALL[text() = 'unlist']
    and expr/SYMBOL[text() = 'seq_len']
  ]",
  lint_message = "Use sequence() to generate a concatenated sequence of seq_len()."
)
