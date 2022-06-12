
#' Extract row by ID
#'
#' @describeIn ids_with_token
#' Return the row of the `parsed_content` entry of the `[get_source_expressions]()` object. Typically used in
#' conjunction with `ids_with_token` to iterate over rows containing desired tokens.
#'
#' @param id Integer. The index corresponding to the desired row
#' of `parsed_content`.
#' @return `with_id`: A data frame corresponding to the row(s) specified in `id`.
#' @export
with_id <- function(source_expression, id, source_file) {
  if (!missing(source_file)) {
    lintr_deprecated(old = "source_file", new = "source_expression", version = "3.0.0", type = "Argument")
    source_expression <- source_file
  }
  if (!is_lint_level(source_expression, "expression")) {
    return(data.frame())
  }
  source_expression$parsed_content[id, ]
}
