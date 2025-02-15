#' Lint common mistakes/style issues cropping up from return statements
#'
#' `return(x <- ...)` is either distracting (because `x` is ignored), or
#'   confusing (because assigning to `x` has some side effect that is muddled
#'   by the dual-purpose expression).
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "foo <- function(x) return(y <- x + 1)",
#'   linters = function_return_linter()
#' )
#'
#' lint(
#'   text = "foo <- function(x) return(x <<- x + 1)",
#'   linters = function_return_linter()
#' )
#'
#' writeLines("e <- new.env() \nfoo <- function(x) return(e$val <- x + 1)")
#' lint(
#'   text = "e <- new.env() \nfoo <- function(x) return(e$val <- x + 1)",
#'   linters = function_return_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "foo <- function(x) return(x + 1)",
#'   linters = function_return_linter()
#' )
#'
#' code_lines <- "
#' foo <- function(x) {
#'   x <<- x + 1
#'   return(x)
#' }
#' "
#' lint(
#'   text = code_lines,
#'   linters = function_return_linter()
#' )
#'
#' code_lines <- "
#' e <- new.env()
#' foo <- function(x) {
#'   e$val <- x + 1
#'   return(e$val)
#' }
#' "
#' writeLines(code_lines)
#' lint(
#'   text = code_lines,
#'   linters = function_return_linter()
#' )
#'
#' @evalRd rd_tags("function_return_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
function_return_linter <- make_linter_from_function_xpath(
  function_names = "return",
  xpath = "parent::expr/expr[LEFT_ASSIGN or RIGHT_ASSIGN]",
  lint_message = "Move the assignment outside of the return() clause, or skip assignment altogether."
)
