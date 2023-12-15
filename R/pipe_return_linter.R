#' Block usage of return() in magrittr pipelines
#'
#' [return()] inside a magrittr pipeline does not actually execute `return()`
#'   like you'd expect: `\(x) { x %>% return(); FALSE }` will return `FALSE`!
#'   It will technically work "as expected" if this is the final statement
#'   in the function body, but such usage is misleading. Instead, assign
#'   the pipe outcome to a variable and return that.
#'
#' @examples
#' # will produce lints
#' lint(
#'   text = "function(x) x %>% return()",
#'   linters = pipe_return_linter()
#' )
#'
#' # okay
#' code <- "function(x) {\n  y <- sum(x)\n  return(y)\n}"
#' writeLines(code)
#' lint(
#'   text = code,
#'   linters = pipe_return_linter()
#' )
#'
#' @evalRd rd_tags("pipe_return_linter")
#' @seealso [linters] for a complete list of linters available in lintr.
#' @export
pipe_return_linter <- make_linter_from_xpath(
  # NB: Native pipe disallows this at the parser level, so there's no need
  #   to lint in valid R code.
  xpath = "
  //SPECIAL[text() = '%>%']
    /following-sibling::expr[expr/SYMBOL_FUNCTION_CALL[text() = 'return']]
  ",
  lint_message = paste(
    "Avoid return() as the final step of a magrittr pipeline. ",
    "Instead, assign the output of the pipeline to a well-named object and return that."
  )
)
