#' Get parsed IDs by token
#'
#' Gets the source IDs (row indices) corresponding to given token.
#'
#' @param source_expression A list of source expressions, the result of a call to [get_source_expressions()], for the desired
#' filename.
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
#' @return `ids_with_token`: The indices of the `parsed_content` data frame
#' entry of the list of source expressions. Indices correspond to the
#' *rows* where `fun` evaluates to `TRUE` for the `value` in the *token* column.
#' @export
ids_with_token <- function(source_expression, value, fun = `==`) {
  if (is.null(source_expression$parsed_content)) {
    return(integer(0))
  }
  loc <- which(fun(source_expression$parsed_content$token, value))
  if (loc %==% integer(0)) {
    return(integer(0))
  }
  loc
}
