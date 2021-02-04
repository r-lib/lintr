
#' Extract row by ID
#'
#' @describeIn ids_with_token
#' Return the row of the `parsed_content` entry of the
#' `\link{get_source_expressions}()` object. Typically used in conjunction with
#' `ids_with_token` to iterate over rows containing desired tokens.
#'
#' @param id Integer. The index corresponding to the desired row
#' of `parsed_content`.
#' @return `with_id`: A data frame corresponding to the row(s) specified in `id`.
#' @export
with_id <- function(source_file, id) {
  if (is.null(source_file$parsed_content)) {
    return(data.frame())
  }
  source_file$parsed_content[id, ]
}
