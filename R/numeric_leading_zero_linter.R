#' Require usage of a leading zero in all fractional numerics
#'
#' While .1 and 0.1 mean the same thing, the latter is easier to read due
#' to the small size of the '.' glyph.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "x <- .1",
#'   linters = numeric_leading_zero_linter()
#' )
#'
#' lint(
#'   text = "x <- -.1",
#'   linters = numeric_leading_zero_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "x <- 0.1",
#'   linters = numeric_leading_zero_linter()
#' )
#'
#' lint(
#'   text = "x <- -0.1",
#'   linters = numeric_leading_zero_linter()
#' )
#'
#' @evalRd rd_tags("numeric_leading_zero_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
numeric_leading_zero_linter <- make_linter_from_xpath(
  # NB:
  #  1. negative constants are split to two components:
  #    OP-MINUS, NUM_CONST
  #  2. complex constants are split to three components:
  #    NUM_CONST, OP-PLUS/OP-MINUS, NUM_CONST
  xpath = "//NUM_CONST[starts-with(text(), '.')]",
  lint_message = "Include the leading zero for fractional numeric constants."
)
