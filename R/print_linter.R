#' Block usage of print() for logging
#'
#' The default print method for character vectors is appropriate for interactively inspecting objects,
#'   not for logging messages. Thus checked-in usage like `print(paste('Data has', nrow(DF), 'rows.'))`
#'   is better served by using [cat()], e.g. `cat(sprintf('Data has %d rows.\n', nrow(DF)))` (noting that
#'   using `cat()` entails supplying your own line returns, and that [glue::glue()] might be preferable
#'   to [sprintf()] for constructing templated strings). Lastly, note that [message()] differs slightly
#'   from `cat()` in that it prints to `stderr` by default, not `stdout`, but is still a good option
#'   to consider for logging purposes.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "print('a')",
#'   linters = print_linter()
#' )
#'
#' lint(
#'   text = "print(paste(x, 'y'))",
#'   linters = print_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "print(x)",
#'   linters = print_linter()
#' )
#'
#' @evalRd rd_tags("print_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
print_linter <- make_linter_from_function_xpath(
  function_names = "print",
  xpath = "
    parent::expr[expr[2][
      STR_CONST
      or expr/SYMBOL_FUNCTION_CALL[
        text() = 'paste' or text() = 'paste0' or text() = 'sprintf'
      ]
    ]]
  ",
  lint_message =
    "Use cat() instead of print() logging messages. Use message() in cases calling for a signalled condition."
)
