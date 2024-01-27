#' Get parsed IDs by token
#'
#' Gets the source IDs (row indices) corresponding to given token.
#'
#' @param source_expression A list of source expressions, the result of a call to [get_source_expressions()],
#'   for the desired filename.
#' @param value Character. String corresponding to the token to search for.
#' For example:
#'
#'  * "SYMBOL"
#'  * "FUNCTION"
#'  * "EQ_FORMALS"
#'  * "$"
#'  * "("
#'
#' @param fun For additional flexibility, a function to search for in
#' the `token` column of `parsed_content`. Typically `==` or `%in%`.
#' @param source_file (DEPRECATED) Same as `source_expression`. Will be removed.
#'
#' @examples
#' tmp <- tempfile()
#' writeLines(c("x <- 1", "y <- x + 1"), tmp)
#' source_exprs <- get_source_expressions(tmp)
#' ids_with_token(source_exprs$expressions[[1L]], value = "SYMBOL")
#' with_id(source_exprs$expressions[[1L]], 2L)
#' unlink(tmp)
#'
#' @return `ids_with_token`: The indices of the `parsed_content` data frame
#' entry of the list of source expressions. Indices correspond to the
#' *rows* where `fun` evaluates to `TRUE` for the `value` in the *token* column.
#' @export
ids_with_token <- function(source_expression, value, fun = `==`, source_file = NULL) {
  if (!missing(source_file)) {
    lintr_deprecated(
      what = "source_file", alternative = "source_expression",
      version = "3.0.0", type = "Argument",
      signal = "stop"
    )
  }
  if (!is_lint_level(source_expression, "expression")) {
    return(integer())
  }
  loc <- which(fun(source_expression$parsed_content$token, value))
  if (loc %==% integer()) {
    return(integer())
  }
  loc
}
