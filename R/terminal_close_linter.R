#' Prohibit close() from terminating a function definition
#'
#' Functions that end in `close(x)` are almost always better written by using
#'   `on.exit(close(x))` close to where `x` is defined and/or opened.
#'
#' @evalRd rd_tags("terminal_close_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
terminal_close_linter <- make_linter_from_xpath(
  xpath = "
  //FUNCTION
    /following-sibling::expr
    /expr[last()][
      expr/SYMBOL_FUNCTION_CALL[text() = 'close']
      or expr[
        SYMBOL_FUNCTION_CALL[text() = 'return']
        and following-sibling::expr/expr/SYMBOL_FUNCTION_CALL[text() = 'close']
      ]
    ]
  ",
  lint_message = paste(
    "Use on.exit(close(x)) to close connections instead of",
    "running it as the last call in a function."
  )
)
